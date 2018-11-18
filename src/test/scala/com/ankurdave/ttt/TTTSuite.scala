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
      Game(YearMonth.of(2017, 5), PlayerId("A"), PlayerId("E"))))

    val skillVariables = new TTT(games).run()

    val sortedSkillVariables = skillVariables.toSeq.sortBy {
      case ((date, player), skill) => (date, player.name)
    }

    val expected = Seq(
      ((YearMonth.of(2017, 1), PlayerId("A")), Gaussian.fromMS(21.904572, 3.085834)),
      ((YearMonth.of(2017, 1), PlayerId("B")), Gaussian.fromMS(12.024438, 3.912764)),
      ((YearMonth.of(2017, 1), PlayerId("C")), Gaussian.fromMS(16.677848, 2.731722)),
      ((YearMonth.of(2017, 2), PlayerId("A")), Gaussian.fromMS(21.910445, 3.121211)),
      ((YearMonth.of(2017, 2), PlayerId("B")), Gaussian.fromMS(12.000478, 3.926290)),
      ((YearMonth.of(2017, 2), PlayerId("C")), Gaussian.fromMS(16.751903, 2.746939)),
      ((YearMonth.of(2017, 3), PlayerId("A")), Gaussian.fromMS(21.916405, 3.156069)),
      ((YearMonth.of(2017, 3), PlayerId("C")), Gaussian.fromMS(16.802012, 2.771747)),
      ((YearMonth.of(2017, 3), PlayerId("D")), Gaussian.fromMS(12.363340, 4.096465)),
      ((YearMonth.of(2017, 4), PlayerId("A")), Gaussian.fromMS(21.922365, 3.190430)),
      ((YearMonth.of(2017, 4), PlayerId("C")), Gaussian.fromMS(16.825754, 2.806108)),
      ((YearMonth.of(2017, 4), PlayerId("E")), Gaussian.fromMS(12.029743, 3.918758)),
      ((YearMonth.of(2017, 5), PlayerId("A")), Gaussian.fromMS(21.928325, 3.224310)),
      ((YearMonth.of(2017, 5), PlayerId("E")), Gaussian.fromMS(12.023805, 3.944079)))

    assert(sortedSkillVariables.size === expected.size)
    for ((((d1, p1), s1), ((d2, p2), s2)) <- sortedSkillVariables.zip(expected)) {
      assert(d1 === d2)
      assert(p1 === p2)
      assert(s1.mu === s2.mu +- 0.000001)
      assert(s1.sigma === s2.sigma +- 0.000001)
    }
  }
}
