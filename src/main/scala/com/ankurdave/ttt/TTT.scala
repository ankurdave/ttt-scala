package com.ankurdave.ttt

import scala.collection.mutable

case class PlayerId(name: String)

case class Game[Date](date: Date, winner: PlayerId, loser: PlayerId)

/** Type class for values that can be incremented and decremented. */
trait IncrementDecrement[A] {
  def prev(a: A): A
  def next(a: A): A
  /** Inclusive range */
  def to(start: A, end: A): Seq[A]
}

/** A container for a set of two-player game results. */
class Games[Date : Ordering](games: Seq[Game[Date]]) {
  private val byDate: Map[Date, Seq[Game[Date]]] = games.groupBy(_.date)

  val dates: Set[Date] = byDate.keySet
  val minDate: Date = dates.min
  val maxDate: Date = dates.max

  /** Filters the games to those at a particular date. */
  def forDate(date: Date): Seq[Game[Date]] = byDate(date)

  /** Returns the start and end date for each player. */
  def playerStartEndDates: Map[PlayerId, (Date, Date)] = {
    val dates =
      for {
        Game(d, w, l) <- games
        p <- Seq(w, l)
      } yield (p, d)
    val datesByPlayer: Map[PlayerId, Seq[Date]] = dates.groupBy(_._1).mapValues(_.map(_._2))
    datesByPlayer.mapValues(dates => (dates.min, dates.max))
  }
}

/** The driver for computing TrueSkill Through Time over a set of two-player game results. */
class TTT[Date : Ordering : IncrementDecrement](
    games: Games[Date],
    mu: Double = 15,
    sigma: Double = 15 / 3.0,
    beta: Double = 15 / 6.0,
    tau: Double = 15 / 30.0,
    delta: Double = 0.01) {

  /** * Returns the computed skill estimate for each player at each date they were active. */
  def run(): Map[(Date, PlayerId), Gaussian] = {
    System.err.println("Building factor graph")
    val (skillVariables, schedule) = buildFactorGraph()
    System.err.println("Running schedule")
    schedule.run()
    System.err.println("Done")
    skillVariables
  }

  /** Constructs a factor graph over player skills and a message passing schedule for inference. */
  private def buildFactorGraph(): (Map[(Date, PlayerId), Gaussian], Schedule) = {
    val skillVariables = mutable.HashMap.empty[(Date, PlayerId), Gaussian]
    val skillDynamicsFactors = mutable.HashMap.empty[(Date, PlayerId), Factor]
    val skillPriorFactors = mutable.HashMap.empty[(Date, PlayerId), Factor]

    for ((p, (s, e)) <- games.playerStartEndDates) {
      for (d <- implicitly[IncrementDecrement[Date]].to(s, e)) {
        skillVariables((d, p)) = Gaussian(0, 0)
        if (d == s) {
          // Prior over new players' skills
          skillPriorFactors((d, p)) = PriorFactor(mu, sigma * sigma, skillVariables((d, p)))
        } else {
          // Dynamics factor between previous skill and current skill
          val dynamicsFactor = LikelihoodFactor(
            skillVariables((d, p)),
            skillVariables((implicitly[IncrementDecrement[Date]].prev(d), p)),
            tau * tau)
          skillDynamicsFactors((d, p)) = dynamicsFactor
        }
      }
    }

    val priorSchedule = ScheduleSeq(skillPriorFactors.values.map(f => ScheduleStep(f, 0)).toSeq)

    val mainSchedule = buildSchedule(
      games.minDate, skillVariables.toMap, skillDynamicsFactors.toMap)

    val fullSchedule = ScheduleLoop(
      ScheduleSeq(priorSchedule.steps ++ mainSchedule.steps), delta, true)

    (skillVariables.toMap, fullSchedule)
  }

  /** Recursively build the message passing schedule at and after a given date. */
  private def buildSchedule(
      date: Date,
      skillVariables: Map[(Date, PlayerId), Gaussian],
      skillDynamicsFactors: Map[(Date, PlayerId), Factor])
    : ScheduleSeq = {
    val forwardDynamicsSchedule = ScheduleSeq(
      (for (((d, p), v) <- skillDynamicsFactors; if d == date)
      yield ScheduleStep(v, 0)).toSeq)

    // Construct the factor subgraph for each game that occurred during this timestep. The factor
    // subgraph links the skill variables of the two players involved in the game.
    val dataSchedule = ScheduleLoop(ScheduleSeq(
      (for {
        Game(_, winner, loser) <- games.forDate(date)

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

        gameFactor = GreaterThanZeroFactor(performanceDifferenceVariable)
      } yield Seq(
        ScheduleStep(winnerPerformanceFactor, 0),
        ScheduleStep(loserPerformanceFactor, 0),
        ScheduleStep(performanceDifferenceFactor, 0),
        ScheduleStep(gameFactor, 0),
        ScheduleStep(performanceDifferenceFactor, 1),
        ScheduleStep(performanceDifferenceFactor, 2),
        ScheduleStep(winnerPerformanceFactor, 1),
        ScheduleStep(loserPerformanceFactor, 1)
      )).flatten), delta, false)

    val nextSchedule =
      if (implicitly[Ordering[Date]].lt(date, games.maxDate)) {
        buildSchedule(
          implicitly[IncrementDecrement[Date]].next(date), skillVariables, skillDynamicsFactors)
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
