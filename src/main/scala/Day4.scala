import States._
import cats.data.{NonEmptyList, State}
import cats.effect.IO
import cats.implicits._
import com.softwaremill.quicklens._

case class Board(grid: List[List[Cell]]) {
  def check(last: Int): Option[Int] =
    if (grid.exists(_.forall(_.marked)) || grid.transpose.exists(
          _.forall(_.marked)
        )) {
      val sum = grid
        .flatMap(_.collect {
          case Cell(value, marked) if !marked => value
        })
        .sum
      Some(sum * last)
    } else
      None
}

object Board {
  def empty: Board = Board(Nil)
}

case class Cell(value: Int, marked: Boolean) {
  def update(called: Int): Cell =
    if (called == value) copy(marked = true) else this
}
object States {
  def updateCell(called: Int): State[Cell, Unit] =
    State.modify(_.update(called))

  /**
    * I wonder if this is best way to build a state which updates all elements in a List?
    * Unfortunately it's the opposite of how State is normally used, which is to carry state DOWN a List
    */
  def updateRow(called: Int): State[List[Cell], Unit] =
    State.modify(_.traverse(updateCell(called).runS).value)

  def updateGrid(called: Int): State[List[List[Cell]], Unit] =
    State.modify(_.traverse(updateRow(called).runS).value)

  /**
    * More interesting, since we have to actually return some useful value from the State
    */
  def updateBoard(called: Int): State[Board, Option[Int]] =
    State
      .modify[Board](_.modify(_.grid).using(updateGrid(called).runS(_).value))
      .inspect(_.check(called))

  def updateBoards(called: Int): State[List[Board], Option[Int]] = State {
    boards =>
      val results = boards.map(updateBoard(called).run(_).value)
      results.collect { case (board, None) => board } -> results.collectFirst {
        case (_, Some(won))                => won
      }
  }
}

/**
  * Cell has a State which returns nothing but updates the cell if it's called
  * Board has a State which returns an option of the score, if finished, after updating all the cells
  * So it needs to sequence/traverse all the cell states to get an updated grid, then check for finished
  * So then the top level list of boards needs a State which traverses the boards and returns
  */
object Day4 extends Day[(List[Int], List[Board]), (List[Int], List[Board])] {
  override def day: Int = 4

  override def parse1(strs: Vector[String]): IO[(List[Int], List[Board])] =
    strs.toList match {
      case row :: grids =>
        IO {
          val ints = row.split(',').toList.map(_.toInt)
          val boards = grids.foldLeft(NonEmptyList.one(Board.empty)) {
            (acc, row) =>
              if (row.isEmpty) Board.empty :: acc
              else {
                val boardRow = row
                  .trim()
                  .replace("  ", " ")
                  .split(' ')
                  .toList //very lazy parsing
                acc
                  .modify(_.head.grid)
                  .using(_ :+ boardRow.map(s => Cell(s.toInt, marked = false)))
              }
          }
          ints -> boards.toList
        }
      case Nil => IO.raiseError(new RuntimeException("nope"))
    }

  override def solve1Impl(input: (List[Int], List[Board])): IO[String] = {

    //And now we can actually do the useful thing! Which is carry the state along the list of called numbers.
    //Need to short circuit though...
    val (numbers, boards) = input
    numbers
      .foldLeft(boards -> Option.empty[Int]) {
        case ((boards, answer), next) =>
          answer match {
            case Some(value) => Nil -> Some(value)
            case None        => updateBoards(next).run(boards).value
          }
      }
      ._2 match {
      case Some(value) => IO.pure(value.toString)
      case None        => IO.pure(s"No Answer!")
    }
  }

  override def parse2(strs: Vector[String]): IO[(List[Int], List[Board])] =
    parse1(strs)

  override def solve2Impl(input: (List[Int], List[Board])): IO[String] = {
    //And now we can actually do the useful thing! Which is carry the state along the list of called numbers.
    //Need to short circuit though...
    val (numbers, boards) = input
    numbers
      .foldLeft(boards -> Option.empty[Int]) {
        case ((boards, latestAnswer), next) =>
          val (nextBoards, maybeNewAnswer) =
            updateBoards(next).run(boards).value
          nextBoards -> maybeNewAnswer.orElse(latestAnswer)
      }
      ._2 match {
      case Some(value) => IO.pure(value.toString)
      case None        => IO.pure(s"No Answer!")
    }
  }
}
