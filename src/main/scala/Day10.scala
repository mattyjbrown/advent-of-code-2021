import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._
import com.softwaremill.quicklens._

import scala.util.Try
import scala.util.control.NoStackTrace

object Day10 extends Day[Vector[String], Vector[String]] {
  final case class BadChar(char: Char, expected: Option[Char])
      extends NoStackTrace
  implicit class CharOps(char: Char) {
    def inverse: Option[Char] = char match {
      case '[' => ']'.some
      case '{' => '}'.some
      case '<' => '>'.some
      case '(' => ')'.some
      case ']' => '['.some
      case '}' => '{'.some
      case '>' => '<'.some
      case ')' => '('.some
      case _   => None
    }

    def isOpen: Boolean =
      char == '[' || char == '{' || char == '<' || char == '('

    def errorValue: Int = char match {
      case ')' => 3
      case ']' => 57
      case '}' => 1197
      case '>' => 25137
      case _   => 0
    }

    def completionValue: Int = char match {
      case ')' => 1
      case ']' => 2
      case '}' => 3
      case '>' => 4
      case _   => 0
    }
  }
  override def day: Int = 10

  override def parse1(strs: Vector[String]): IO[Vector[String]] = IO.pure(strs)

  override def solve1Impl(input: Vector[String]): IO[String] =
    input
      .traverse { line =>
        IO {
          line.foldLeft(List.empty[Char]) { (acc, char) =>
            acc match {
              case head :: rest if char.isOpen =>
                char :: head :: rest //If it's an opening char, it can always go on
              case head :: rest if head.inverse.contains(char) =>
                rest //It's a close char. Remove most recent
              case head :: rest =>
                throw BadChar(char, head.inverse) //It's a close, and it isn't the most recent open
              case Nil => char :: Nil
            }
          }
        }.attempt
      }
      .map(_.foldLeft(0) { (total, next) =>
        next match {
          case Left(BadChar(got, expected)) => total + got.errorValue
          case Right(remaining)             => total + 0
        }
      })
      .map(_.toString)

  override def parse2(strs: Vector[String]): IO[Vector[String]] = parse1(strs)

  override def solve2Impl(input: Vector[String]): IO[String] =
    input
      .traverse { line =>
        IO {
          line.foldLeft(List.empty[Char]) { (acc, char) =>
            acc match {
              case head :: rest if char.isOpen =>
                char :: head :: rest //If it's an opening char, it can always go on
              case head :: rest if head.inverse.contains(char) =>
                rest //It's a close char. Remove most recent
              case head :: rest =>
                throw BadChar(char, head.inverse) //It's a close, and it isn't the most recent open
              case Nil => char :: Nil
            }
          }
        }.attempt
      }
      .map(_.collect {
        case Right(remaining) => remaining.flatMap(_.inverse)
      })
      .map(
        _.map(
          _.foldLeft(0L)((score, next) => (score * 5) + next.completionValue)
        )
      )
      .map { scores =>
        def median(ints: Vector[Long]): Long = {
          ints match {
            case Vector(last) => last
            case Vector(a, b) => (a + b) / 2
            case more         => median(more.init.tail)
          }
        }
        println(scores)
        median(scores.sorted).toString
      }
}
