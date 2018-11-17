package com.ankurdave.ttt

sealed trait Schedule {
  def run(): Double
}

case class ScheduleStep(factor: Factor, index: Int) extends Schedule {
  override def run(): Double = factor.updateMessage(index)
}

case class ScheduleSeq(steps: Seq[Schedule]) extends Schedule {
  override def run(): Double =
    if (steps.size > 0) steps.map(_.run()).max else 0.0
}

case class ScheduleLoop(child: Schedule, delta: Double) extends Schedule {
  override def run(): Double = {
    var i = 0
    var curDelta = child.run()
    while (curDelta > delta) {
      println("ScheduleLoop iteration " + i)
      curDelta = child.run()
      i += 1
    }
    curDelta
  }
}
