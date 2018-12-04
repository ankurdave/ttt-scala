package com.ankurdave.ttt

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.Mode
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.State
import org.openjdk.jmh.infra.Blackhole

import java.time.YearMonth
import com.ankurdave.ttt.implicits._

@State(Scope.Thread)
class TTTBenchmark {
  val gamesFile = io.Source.fromFile("squash.csv")
  val games =
    try {
      new Games(
        (for (line <- gamesFile.getLines) yield {
          val cols = line.split(",").map(_.trim)
          val dateInt = cols(1).toInt
          val date = YearMonth.of(dateInt / 12, dateInt % 12 + 1)
          val winner = cols(2)
          val loser = cols(3)
          Game(date, PlayerId(winner), PlayerId(loser))
        }).toSeq)
    } finally {
      gamesFile.close()
    }

  @Benchmark
  @BenchmarkMode(Array(Mode.SingleShotTime))
  def buildAndRunSchedule(bh: Blackhole): Unit = {
    bh.consume(new TTT(games, 3.5, 3.5 / 0.7, 3.5 / 0.7 * 0.37, 3.5 / 0.7 * 0.058).run())
  }
}
