import cats.effect.IO

object Day1 extends Day {
  def solve1Impl(strs: Vector[String]): IO[String] = {
    val res = strs.map(_.toInt).sliding(2).foldLeft(0) { (acc, next) =>
      if (next.last > next.head) acc + 1
      else acc
    }
    IO.pure(res.toString)
  }

  def solve2Impl(strs: Vector[String]): IO[String] = {
    val res = strs.map(_.toInt).sliding(3).map(_.sum).sliding(2).foldLeft(0) { (acc, next) =>
      if (next.last > next.head) acc + 1
      else acc
    }
    IO.pure(res.toString)
  }

  override def day: Int = 1
}
