# TrueSkill Through Time in Scala

This is a port of MSR Cambridge's [TrueSkill Through Time](https://papers.nips.cc/paper/3331-trueskill-through-time-revisiting-the-history-of-chess.pdf) algorithm from [F#](https://github.com/lucasmaystre/ChessAnalysis) to Scala. The port simplifies the code for use in the [Cal Squash box league rankings](https://github.com/ankurdave/calsquash-rankings).

Example usage (try it out with `sbt console`):

```scala
import com.ankurdave.ttt._
import com.ankurdave.ttt.implicits._
import java.time.YearMonth

val games = new Games(Seq(
  Game(YearMonth.of(2017, 1), PlayerId("A"), PlayerId("B")),
  Game(YearMonth.of(2017, 1), PlayerId("C"), PlayerId("D")),
  Game(YearMonth.of(2017, 1), PlayerId("E"), PlayerId("F")),
  Game(YearMonth.of(2017, 2), PlayerId("B"), PlayerId("C")),
  Game(YearMonth.of(2017, 2), PlayerId("D"), PlayerId("E"))))

val skillVariables = new TTT(games).run()

skillVariables((YearMonth.of(2017, 2), PlayerId("B")))
// res0: com.ankurdave.ttt.Gaussian = Gaussian(mu=16.374122, sigma=3.501964)

skillVariables((YearMonth.of(2017, 2), PlayerId("D")))
// res1: com.ankurdave.ttt.Gaussian = Gaussian(mu=14.698318, sigma=3.375058)
```
