import cats.data.State
import cats.effect.IO
import cats.implicits._
import com.softwaremill.quicklens._
import Day11Utils._
import scala.annotation.tailrec

object Day11Utils {
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
        coords.modify(_.y).using(_ - 1),
        coords.modify(_.x).using(_ + 1).modify(_.y).using(_ + 1),
        coords.modify(_.x).using(_ + 1).modify(_.y).using(_ - 1),
        coords.modify(_.x).using(_ - 1).modify(_.y).using(_ + 1),
        coords.modify(_.x).using(_ - 1).modify(_.y).using(_ - 1)
      ).filter(at(_).isDefined) //lazy

    lazy val lastFlashers: Vector[Coords] = all.filter(at(_).contains(10))

    override lazy val toString: String = grid.map(_.mkString).mkString("\n")
  }

  val incrementAll: State[Grid, Int] = State { grid =>
    val newGrid = grid.copy(grid = grid.grid.map(_.map(_ + 1)))
    newGrid -> newGrid.grid.map(_.count(_ == 10)).sum
  }

  val neighbourFlash: State[Grid, Int] = State { grid =>
    @tailrec
    def flashRec(internalGrid: Grid,
                 toFlash: List[Coords],
                 iteration: Int): (Grid, Int) = toFlash match {
      case Nil => internalGrid -> iteration
      case thisFlash :: rest =>
        val toIncrement = thisFlash +: internalGrid.neighbours(thisFlash)
        val mutable = internalGrid.grid.map(_.toArray).toArray
        toIncrement.foreach {
          case Coords(x, y) => mutable(x)(y) = mutable(x)(y) + 1
        }
        val nextGrid = Grid(mutable.map(_.toVector).toVector)
        flashRec(
          nextGrid,
          (rest ++ nextGrid.lastFlashers).distinct,
          iteration + 1
        )
    }
    flashRec(grid, grid.lastFlashers.toList, 0)
  }

  val zeroAll: State[Grid, Unit] = State.modify { grid =>
    grid.copy(grid = grid.grid.map(_.map(i => if (i > 9) 0 else i)))
  }

  val cycle: State[Grid, Int] =
    for {
      _ <- incrementAll
      flashes <- neighbourFlash
      _ <- zeroAll
    } yield flashes
}

object Day11 extends Day[Grid, Grid] {
  override def day: Int = 11

  override def parse1(strs: Vector[String]): IO[Grid] =
    IO {
      strs.map(rowString => rowString.map(_.toString.toInt).to(Vector))
    }.map(Grid)

  override def solve1Impl(input: Grid): IO[String] = {
    val steps = 100
    val (finalGrid, flashes) = List.fill(steps)(cycle).sequence.run(input).value
    val answer = flashes.sum
    println(finalGrid)
    println(flashes)
    IO.pure(answer.toString)
  }

  override def parse2(strs: Vector[String]): IO[Grid] = parse1(strs)

  override def solve2Impl(input: Grid): IO[String] = {
    val steps = 500
    val flashes = List.fill(steps)(cycle).sequence.runA(input).value
    val answer = flashes.indexWhere(_ == input.all.size) + 1
    println(flashes)
    IO.pure(answer.toString)
  }
}
