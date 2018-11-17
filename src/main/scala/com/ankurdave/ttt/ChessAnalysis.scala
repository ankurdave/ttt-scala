package com.ankurdave.ttt

import scala.io.Source
import java.time.YearMonth
import java.io.PrintWriter
import java.io.File

object ChessAnalysis extends App {
  val file = args(1)
  val source = Source.fromFile(file)
  val ms = 
    for {
      line <- source.getLines
      fields = line.split(",")
      dateInt = fields(1).toInt
      month = dateInt % 12
      year = (dateInt - month) / 12
      winner = fields(2)
      loser = fields(3)
    } yield (Date(YearMonth.of(year, month + 1)), PlayerId(winner), PlayerId(loser))
  val matches = new Matches(ms.toSeq)
  source.close()

  val pw = new PrintWriter(new File("output.csv"))
  for (((d, p), skill) <- new TTT(matches).run().skillVariables.toSeq.sortBy(_._1._1.ym)) {
    pw.write("%s,%d,%f,%f\n".format(p, d.ym.getYear * 12 + d.ym.getMonthValue, skill.mu, skill.sigma))
  }
  pw.close()
}
