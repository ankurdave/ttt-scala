package com.ankurdave.ttt

import scala.collection.mutable

import java.time.YearMonth

case class PlayerId(name: String)

object Date {
  def range(start: Date, end: Date): Seq[Date] = {
    val result = mutable.ArrayBuffer.empty[Date]
    var cur = start
    while (cur < end) {
      result += cur
      cur = cur.next()
    }
    result += end
  }
}

case class Date(ym: YearMonth) {
  def prev(): Date = Date(ym.minusMonths(1))
  def next(): Date = Date(ym.plusMonths(1))
  def <(other: Date): Boolean = ym.isBefore(other.ym)
}

case class SkillHistory(matches: Matches, skillVariables: Map[(Date, PlayerId), Gaussian])

class Matches(data: Seq[(Date, PlayerId, PlayerId)]) {

  def byDate(date: Date): Seq[(PlayerId, PlayerId)] =
    for {
      (d, w, l) <- data
      if d == date
    } yield (w, l)

  def playerStartEndDates: Map[PlayerId, (Date, Date)] = {
    val dates =
      for {
        (d, w, l) <- data
        p <- Seq(w, l)
      } yield (p, d)
    val datesByPlayer: Map[PlayerId, Seq[Date]] = dates.groupBy(_._1).mapValues(_.map(_._2))
    datesByPlayer.mapValues(dates => (dates.minBy(_.ym), dates.maxBy(_.ym)))
  }

  val dates = data.map(_._1).toSet

  val minDate: Date = dates.minBy(_.ym)
  val maxDate: Date = dates.maxBy(_.ym)
}

class TTT(
    matches: Matches,
    mu: Double = 18,
    sigma: Double = 18 / 3.0,
    beta: Double = 18 / 6.0,
    tau: Double = 18 / 30.0,
    delta: Double = 0.01) {

  val tauSquared = tau * tau

  def run(): SkillHistory = {
    println("Building factor graph")
    val (skillVariables, schedule) = buildFactorGraph()
    println("Running")
    schedule.run()
    println("Done")
    new SkillHistory(matches, skillVariables)
  }

  private def buildFactorGraph(): (Map[(Date, PlayerId), Gaussian], Schedule) = {
    val skillVariables = mutable.HashMap.empty[(Date, PlayerId), Gaussian]
    val skillDynamicsFactors = mutable.HashMap.empty[(Date, PlayerId), Factor]
    val skillPriorFactors = mutable.HashMap.empty[(Date, PlayerId), Factor]

    for ((p, (s, e)) <- matches.playerStartEndDates) {
      for (d <- Date.range(s, e)) {
        skillVariables((d, p)) = Gaussian(0, 0)
        if (d == s) {
          // Prior over new players' skills
          skillPriorFactors((d, p)) = PriorFactor(mu, sigma, skillVariables((d, p)))
        } else {
          // Dynamics factor between previous skill and current skill
          val dynamicsFactor = LikelihoodFactor(
            skillVariables((d, p)),
            skillVariables((d.prev(), p)),
            tauSquared)
          skillDynamicsFactors((d, p)) = dynamicsFactor
        }
      }
    }

    val priorSchedule = ScheduleSeq(skillPriorFactors.values.map(f => ScheduleStep(f, 0)).toSeq)

    val mainSchedule = buildSchedule(
      matches.minDate, skillVariables.toMap, skillDynamicsFactors.toMap)

    val fullSchedule = ScheduleLoop(
      ScheduleSeq(priorSchedule.steps ++ mainSchedule.steps), delta)

    (skillVariables.toMap, fullSchedule)
  }

  private def buildSchedule(
      date: Date,
      skillVariables: Map[(Date, PlayerId), Gaussian],
      skillDynamicsFactors: Map[(Date, PlayerId), Factor])
    : ScheduleSeq = {
    println("Building schedule for " + date)

    val forwardDynamicsSchedule = ScheduleSeq(
      (for (((d, p), v) <- skillDynamicsFactors; if d == date)
      yield ScheduleStep(v, 0)).toSeq)

    // Construct the factor subgraph for each match that occurred during this timestep. The factor
    // subgraph links the skill variables of the two players involved in the match.
    val dataSchedule = ScheduleLoop(ScheduleSeq(
      (for {
        (winner, loser) <- matches.byDate(date)

        winnerSkillVariable = skillVariables((date, winner))
        winnerPerformanceVariable = Gaussian(0, 0)
        winnerPerformanceFactor = LikelihoodFactor(
          winnerPerformanceVariable, winnerSkillVariable, beta * beta)

        loserSkillVariable = skillVariables((date, loser))
        loserPerformanceVariable = Gaussian(0, 0)
        loserPerformanceFactor = LikelihoodFactor(
          loserPerformanceVariable, loserSkillVariable, beta * beta)

        performanceDifferenceVariable = Gaussian(0, 0)
        performanceDifferenceFactor = WeightedSumFactor(
          performanceDifferenceVariable, winnerPerformanceVariable, loserPerformanceVariable, 1, -1)

        matchFactor = GreaterThanZeroFactor(performanceDifferenceVariable)
      } yield Seq(
        ScheduleStep(winnerPerformanceFactor, 0),
        ScheduleStep(loserPerformanceFactor, 0),
        ScheduleStep(performanceDifferenceFactor, 0),
        ScheduleStep(matchFactor, 0),
        ScheduleStep(performanceDifferenceFactor, 1),
        ScheduleStep(performanceDifferenceFactor, 2),
        ScheduleStep(winnerPerformanceFactor, 1),
        ScheduleStep(loserPerformanceFactor, 1)
      )).flatten), delta)

    val nextSchedule =
      if (date < matches.maxDate) {
        buildSchedule(date.next(), skillVariables, skillDynamicsFactors)
      } else {
        ScheduleSeq(Seq.empty)
      }

    val backwardDynamicsSchedule = ScheduleSeq(
      (for (((d, p), v) <- skillDynamicsFactors; if d == date)
      yield ScheduleStep(v, 1)).toSeq)

    ScheduleSeq(Seq(
      forwardDynamicsSchedule, dataSchedule, nextSchedule, dataSchedule, backwardDynamicsSchedule))
  }
}
