import cats.effect.IO
import cats.implicits._

object Day1 extends Day[Vector[Int], Vector[Int]] {
  override def day: Int = 1

  override def parse1(strs: Vector[String]): IO[Vector[Int]] =
    strs.traverse(s => IO(s.toInt))

  override def solve1Impl(input: Vector[Int]): IO[String] = {
    val res = input.sliding(2).foldLeft(0) { (acc, next) =>
      if (next.last > next.head) acc + 1
      else acc
    }
    IO.pure(res.toString)
  }

  override def parse2(strs: Vector[String]): IO[Vector[Int]] =
    strs.traverse(s => IO(s.toInt))

  override def solve2Impl(input: Vector[Int]): IO[String] = {
    val res = input.sliding(3).map(_.sum).sliding(2).foldLeft(0) {
      (acc, next) =>
        if (next.last > next.head) acc + 1
        else acc
    }
    IO.pure(res.toString)
  }
}
