package com.ankurdave.ttt

/** A schedule for message passing (expectation propagation) over a factor graph. */
sealed trait Schedule {
  /** Runs the schedule and returns the maximum resulting change of any variable. */
  def run(): Double
}

/** A single step of EP at the given factor in the given direction. */
case class ScheduleStep(factor: Factor, index: Int) extends Schedule {
  override def run(): Double = factor.updateMessage(index)
}

/** A sequence of EP schedules. */
case class ScheduleSeq(steps: Seq[Schedule]) extends Schedule {
  override def run(): Double = {
    var max = 0.0
    for (step <- steps) {
      val delta = step.run()
      if (delta > max) {
        max = delta
      }
    }
    max
  }
}

/** Iterated EP until convergence. Prints at each iteration if `profile` is true. */
case class ScheduleLoop(child: Schedule, delta: Double, profile: Boolean) extends Schedule {
  override def run(): Double = {
    var i = 0
    var curDelta = child.run()
    while (curDelta > delta) {
      if (profile) System.err.println("ScheduleLoop iteration %d, delta=%f".format(i, curDelta))
      curDelta = child.run()
      i += 1
    }
    if (profile) System.err.println(
      "ScheduleLoop done. %d iterations, final delta=%f".format(i, curDelta))
    curDelta
  }
}
