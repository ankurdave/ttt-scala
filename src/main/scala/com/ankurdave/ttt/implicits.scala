package com.ankurdave.ttt

import java.time.YearMonth

import scala.collection.mutable

package object implicits {
  implicit object YearMonthIncrementDecrement extends IncrementDecrement[YearMonth] {
    override def prev(ym: YearMonth): YearMonth = ym.minusMonths(1)
    override def next(ym: YearMonth): YearMonth = ym.plusMonths(1)
    override def to(start: YearMonth, end: YearMonth): Seq[YearMonth] = {
      val result = mutable.ArrayBuffer.empty[YearMonth]
      var cur = start
      while (cur.isBefore(end)) {
        result += cur
        cur = next(cur)
      }
      result += end
    }
  }

  implicit object YearMonthOrdering extends Ordering[YearMonth] {
    override def compare(x: YearMonth, y: YearMonth): Int = x.compareTo(y)
  }
}
