import cats.effect.IO

object Day1 {
  def solve1: IO[Int] =
    Parser.parse(1)
      .map(_.map(_.toInt))
      .map(_.sliding(2).foldLeft(0) { (acc, next) =>
    if (next.last > next.head) acc + 1
    else acc
  })

  def solve2: IO[Int] =
    Parser.parse(1)
      .map(_.map(_.toInt))
      .map(_.sliding(3).map(_.sum).sliding(2).foldLeft(0) { (acc, next) =>
    if (next.last > next.head) acc + 1
    else acc
  })
}
