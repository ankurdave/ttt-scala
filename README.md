# TrueSkill Through Time in Scala

This is a port of MSR Cambridge's [TrueSkill Through Time](https://papers.nips.cc/paper/3331-trueskill-through-time-revisiting-the-history-of-chess.pdf) algorithm from [F#](https://github.com/lucasmaystre/ChessAnalysis) to Scala. The port simplifies the code for use in the [Cal Squash box league rankings](https://github.com/ankurdave/calsquash-rankings).

Example usage (run with `sbt test`):

```scala
val matches = new Matches(Seq(
  (Date(YearMonth.of(2017, 1)), PlayerId("A"), PlayerId("B")),
  (Date(YearMonth.of(2017, 1)), PlayerId("A"), PlayerId("C")),
  (Date(YearMonth.of(2017, 2)), PlayerId("C"), PlayerId("B")),
  (Date(YearMonth.of(2017, 3)), PlayerId("C"), PlayerId("D")),
  (Date(YearMonth.of(2017, 4)), PlayerId("C"), PlayerId("E")),
  (Date(YearMonth.of(2017, 5)), PlayerId("A"), PlayerId("E"))))

for (elem <- new TTT(matches).run().skillVariables.toSeq.sortBy(_._1._1.ym)) {
  println(elem)
}

```
