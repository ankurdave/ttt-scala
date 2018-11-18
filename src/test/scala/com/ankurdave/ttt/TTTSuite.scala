package com.ankurdave.ttt

import java.time.YearMonth

import org.scalatest.FunSuite

import com.ankurdave.ttt.implicits._

import org.scalactic.Tolerance._

class TTTSuite extends FunSuite {
  test("TTT") {
    val games = new Games(Seq(
      Game(YearMonth.of(2017, 1), PlayerId("A"), PlayerId("B")),
      Game(YearMonth.of(2017, 1), PlayerId("A"), PlayerId("C")),
      Game(YearMonth.of(2017, 1), PlayerId("A"), PlayerId("C")),
      Game(YearMonth.of(2017, 1), PlayerId("A"), PlayerId("C")),
      Game(YearMonth.of(2017, 2), PlayerId("C"), PlayerId("B")),
      Game(YearMonth.of(2017, 3), PlayerId("C"), PlayerId("D")),
      Game(YearMonth.of(2017, 4), PlayerId("C"), PlayerId("E")),
      Game(YearMonth.of(2017, 6), PlayerId("A"), PlayerId("E"))))

    val skillVariables = new TTT(games).run()

    val sortedSkillVariables = skillVariables.toSeq.sortBy {
      case ((date, player), skill) => (date, player.name)
    }

    val expected = Seq(
      ((YearMonth.of(2017, 1), PlayerId("A")), Gaussian.fromMS(21.906757, 3.086370)),
      ((YearMonth.of(2017, 1), PlayerId("B")), Gaussian.fromMS(12.024722, 3.912884)),
      ((YearMonth.of(2017, 1), PlayerId("C")), Gaussian.fromMS(16.678273, 2.731999)),
      ((YearMonth.of(2017, 2), PlayerId("A")), Gaussian.fromMS(21.912678, 3.121752)),
      ((YearMonth.of(2017, 2), PlayerId("B")), Gaussian.fromMS(12.000762, 3.926409)),
      ((YearMonth.of(2017, 2), PlayerId("C")), Gaussian.fromMS(16.752309, 2.747215)),
      ((YearMonth.of(2017, 3), PlayerId("A")), Gaussian.fromMS(21.918685, 3.156615)),
      ((YearMonth.of(2017, 3), PlayerId("C")), Gaussian.fromMS(16.802399, 2.772023)),
      ((YearMonth.of(2017, 3), PlayerId("D")), Gaussian.fromMS(12.363464, 4.096519)),
      ((YearMonth.of(2017, 4), PlayerId("A")), Gaussian.fromMS(21.924692, 3.190981)),
      ((YearMonth.of(2017, 4), PlayerId("C")), Gaussian.fromMS(16.826125, 2.806384)),
      ((YearMonth.of(2017, 4), PlayerId("E")), Gaussian.fromMS(12.026724, 3.919606)),
      ((YearMonth.of(2017, 5), PlayerId("A")), Gaussian.fromMS(21.930700, 3.224866)),
      ((YearMonth.of(2017, 5), PlayerId("E")), Gaussian.fromMS(12.020738, 3.944935)),
      ((YearMonth.of(2017, 6), PlayerId("A")), Gaussian.fromMS(21.936707, 3.258286)),
      ((YearMonth.of(2017, 6), PlayerId("E")), Gaussian.fromMS(12.014730, 3.969986)))

    assert(sortedSkillVariables.size === expected.size)
    for ((((d1, p1), s1), ((d2, p2), s2)) <- sortedSkillVariables.zip(expected)) {
      assert(d1 === d2)
      assert(p1 === p2)
      assert(s1.mu === s2.mu +- 0.000001)
      assert(s1.sigma === s2.sigma +- 0.000001)
    }
  }
}
