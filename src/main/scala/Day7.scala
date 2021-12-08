import cats.effect.IO
import cats.implicits._

object Day7 extends Day[List[Int], List[Int]] {
  override def day: Int = 7

  override def parse1(strs: Vector[String]): IO[List[Int]] =
    IO(strs.head.split(",").map(_.toInt).toList)

  override def solve1Impl(input: List[Int]): IO[String] = {
    val sorted = input.sorted.toVector
    val range = sorted.head to sorted.last
    val answers = for (x <- range) yield sorted.map(xn => Math.abs(x - xn)).sum
    IO.pure(answers.min.toString)
  }

  override def parse2(strs: Vector[String]): IO[List[Int]] = parse1(strs)

  override def solve2Impl(input: List[Int]): IO[String] = {
    val sorted = input.sorted.toVector
    val range = sorted.head to sorted.last
    val answers = for (x <- range)
      yield
        sorted.map { xn =>
          val diff = Math.abs(x - xn)
          //sum = n (first + last) / 2
          (diff * (1 + diff)) / 2
        }.sum
    IO.pure(answers.min.toString)
  }
}
