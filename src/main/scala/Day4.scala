import Day4.Board
import cats.effect.IO
import cats.implicits._

object Day4 extends Day[(List[Int], List[Board]), Vector[String]] {
  case class Board()
  override def day: Int = 4

  override def parse1(strs: Vector[String]): IO[(List[Int], List[Board])] = ???

  override def solve1Impl(input: (List[Int], List[Board])): IO[String] = ???

  override def parse2(strs: Vector[String]): IO[Vector[String]] = ???

  override def solve2Impl(input: Vector[String]): IO[String] = ???
}
