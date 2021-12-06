import cats.effect.IO
import cats.implicits._

case class Point(x: Int, y: Int)
object Point {
  def apply(str: String): IO[Point] = str.indexOf(',') match {
    case -1 => IO.raiseError(new RuntimeException(s"Nope: $str"))
    case real =>
      IO(str.splitAt(real).bimap(_.toInt, _.tail.toInt)).map {
        case (x, y) => Point(x, y)
      }
  }
}

object Day5 extends Day[Vector[(Point, Point)], Vector[(Point, Point)]] {
  override def day: Int = 5

  override def parse1(strs: Vector[String]): IO[Vector[(Point, Point)]] =
    strs
      .map(_.split(" -> ") match {
        case Array(first, second) => Point(first).product(Point(second))
        case _                    => IO.raiseError(new RuntimeException(s"Nope"))
      })
      .sequence

  override def solve1Impl(input: Vector[(Point, Point)]): IO[String] = IO {
    input
      .collect {
        case (Point(x1, y1), Point(x2, y2)) if x1 == x2 =>
          val step = if (y1 < y2) 1 else -1
          for (y <- y1.to(y2, step)) yield Point(x1, y)
        case (Point(x1, y1), Point(x2, y2)) if y1 == y2 =>
          val step = if (x1 < x2) 1 else -1
          for (x <- x1.to(x2, step)) yield Point(x, y1)
      }
      .flatten
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .count { case (_, count) => count > 1 }
      .toString
  }

  override def parse2(strs: Vector[String]): IO[Vector[(Point, Point)]] =
    parse1(strs)

  override def solve2Impl(input: Vector[(Point, Point)]): IO[String] = IO {
    input
      .collect {
        case (Point(x1, y1), Point(x2, y2)) if x1 == x2 =>
          val step = if (y1 < y2) 1 else -1
          for (y <- y1.to(y2, step)) yield Point(x1, y)
        case (Point(x1, y1), Point(x2, y2)) if y1 == y2 =>
          val step = if (x1 < x2) 1 else -1
          for (x <- x1.to(x2, step)) yield Point(x, y1)
        case (Point(x1, y1), Point(x2, y2))
            if Math.abs(x1 - x2) == Math.abs(y1 - y2) =>
          val diff = Math.abs(x1 - x2)
          val xStep = if (x1 < x2) 1 else -1
          val yStep = if (y1 < y2) 1 else -1
          for (s <- 0 to diff) yield Point(x1 + s * xStep, y1 + s * yStep)
      }
      .flatten
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .count { case (_, count) => count > 1 }
      .toString
  }
}
