import cats.CommutativeFlatMap.ops.toAllCommutativeFlatMapOps
import cats.effect.IO
import cats.implicits._
import com.softwaremill.quicklens._

case class Coords(x: Int, y: Int)
case class Grid(grid: Vector[Vector[Int]]) {
  require(grid.forall(_.length == grid.head.length))
  val size: Coords = Coords(grid.length, grid.head.length)
  val all: Vector[Coords] =
    (for {
      x <- 0 until size.x
      y <- 0 until size.y
    } yield Coords(x, y)).to(Vector)

  def at(coords: Coords): Option[Int] = at(coords.x, coords.y)

  def at(x: Int, y: Int): Option[Int] =
    for {
      row <- grid.lift(x)
      cell <- row.lift(y)
    } yield cell

  def neighbours(coords: Coords): Vector[Coords] =
    Vector(
      coords.modify(_.x).using(_ + 1),
      coords.modify(_.x).using(_ - 1),
      coords.modify(_.y).using(_ + 1),
      coords.modify(_.y).using(_ - 1)
    ).filter(at(_).isDefined) //lazy

  def lowPoints: Vector[Coords] =
    all
      .map(coords => coords -> neighbours(coords))
      .filter {
        case (coords, neighbours) =>
          val height = at(coords).get
          neighbours.flatMap(at).forall(_ > height)
      }
      .map(_._1)
}

object Day9 extends Day[Grid, Grid] {
  override def day: Int = 9

  override def parse1(strs: Vector[String]): IO[Grid] =
    IO {
      strs.map(rowString => rowString.map(_.toString.toInt).to(Vector))
    }.map(Grid)

  override def solve1Impl(input: Grid): IO[String] =
    input.lowPoints
      .flatMap(input.at)
      .map(_ + 1)
      .sum
      .toString
      .pure[IO]

  override def parse2(strs: Vector[String]): IO[Grid] = parse1(strs)

  override def solve2Impl(input: Grid): IO[String] = {
    def rec(point: Coords): Vector[Coords] = {
      val height = input.at(point).get
      val neighbours = input.neighbours(point)
      val keep = neighbours.filter(input.at(_).exists(h => h > height && h < 9))
      Vector(point) ++ keep ++ keep.flatMap(rec)
    }

    val basins = input.lowPoints.map(rec(_).distinct)
    val answer = basins.map(_.size).sorted.reverse.take(3).product
    IO.pure(answer.toString)
  }
}
