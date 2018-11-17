package com.ankurdave.ttt

import org.scalatest.FunSuite

import java.time.YearMonth

class TTTSuite extends FunSuite {
  test("TTT") {
    val matches = new Matches(Seq(
      (Date(YearMonth.of(2017, 1)), PlayerId("A"), PlayerId("B")),
      (Date(YearMonth.of(2017, 1)), PlayerId("A"), PlayerId("C")),
      (Date(YearMonth.of(2017, 2)), PlayerId("C"), PlayerId("B")),
      (Date(YearMonth.of(2017, 3)), PlayerId("C"), PlayerId("D")),
      (Date(YearMonth.of(2017, 4)), PlayerId("C"), PlayerId("E"))))
    for (elem <- new TTT(matches, 18).run().skillVariables.toSeq.sortBy(_._1._1.ym)) {
      println(elem)
    }
  }
}
