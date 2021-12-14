import cats.data.State
import cats.effect.IO
import cats.implicits._
import com.softwaremill.quicklens._
import Day13Utils._

import scala.annotation.tailrec

object Day13Utils {
  case class Dot(x: Int, y: Int)
  sealed trait Fold
  case class Horizontal(y: Int) extends Fold
  case class Vertical(x: Int) extends Fold
  case class Data(dots: Vector[Dot], folds: List[Fold]) {
    def fold: Data = folds match {
      case h :: t =>
        val newDots = h match {
          case Horizontal(at) =>
            dots.map {
              case Dot(x, y) =>
                val dot = if (y > at) Dot(x, y - 2 * (y - at)) else Dot(x, y)
                println(s"${Dot(x, y)} folded at $h gives $dot")
                dot
            }
          case Vertical(at) =>
            dots.map {
              case Dot(x, y) =>
                val dot = if (x > at) Dot(x - 2 * (x - at), y) else Dot(x, y)
                println(s"${Dot(x, y)} folded at $h gives $dot")
                dot
            }
        }
        Data(newDots.distinct, t)
      case Nil => this
    }
  }
}

object Day13 extends Day[Data, Data] {
  override def day: Int = 13

  override def parse1(strs: Vector[String]): IO[Data] =
    IO {
      val i = strs.indexWhere(_.isEmpty)
      val (dotsS, foldsS) = strs.splitAt(i)
      val dots = dotsS.map { s =>
        val (x, y) = s.splitAt(s.indexOf(",")).bimap(_.toInt, _.tail.toInt)
        Dot(x, y)
      }
      val folds = foldsS.toList.tail.map { s =>
        s.stripPrefix("fold along ").toList match {
          case 'x' :: '=' :: int => Vertical(int.mkString.toInt)
          case 'y' :: '=' :: int => Horizontal(int.mkString.toInt)
          case other             => throw new RuntimeException(s"Huh? $other ($s)")
        }
      }
      Data(dots, folds)
    }

  override def solve1Impl(input: Data): IO[String] =
    IO.pure(input.fold.dots.length.toString)

  override def parse2(strs: Vector[String]): IO[Data] = parse1(strs)

  override def solve2Impl(input: Data): IO[String] = {

    @tailrec
    def rec(oldData: Data): Data = {
      val newData = oldData.fold
      if (oldData == newData) oldData else rec(newData)
    }
    val folded = rec(input)
    val maxx = folded.dots.maxBy(_.x).x
    println(s"$maxx")
    val maxy = folded.dots.maxBy(_.y).y
    println(s"$maxy")
    IO(for {
      y <- 0 to maxy
    } yield {
      val row =
        (0 to maxx).map(x => if (folded.dots.contains(Dot(x, y))) '#' else '.')
      row.mkString
    }).map(_.mkString("\n").prepended('\n'))
  }
}
