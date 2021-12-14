import cats.data.State
import cats.effect.IO
import cats.implicits._
import com.softwaremill.quicklens._
import Day14Utils._

import scala.annotation.tailrec

object Day14Utils {
  type Input = (String, List[PairInsertion])
  case class PairInsertion(pair: (Char, Char), insert: (Char))
}

object Day14 extends Day[Input, Input] {
  override def day: Int = 14

  override def parse1(strs: Vector[String]): IO[Input] = {
    val template = strs.head
    val pairs = strs.tail.tail
    val inserts = pairs.map { s =>
      val insert = s.last
      val pair = s.init.stripSuffix(" -> ")
      PairInsertion(pair.head -> pair.last, insert)
    }
    IO.pure(template, inserts.toList)
  }

  override def solve1Impl(input: Input): IO[String] = {
    val steps = 10
    @tailrec
    def recStep(template: String, iteration: Int): String =
      if (iteration == steps) template
      else {
        val next = template
          .sliding(2)
          .map(p => p.head -> p.last) //As a list of pairs
          .map { pair =>
            input._2.find(_.pair == pair) match {
              case Some(PairInsertion(_, insert)) =>
                List(pair._1, insert).mkString
              case None => pair._1.toString
            }
          }
          .mkString + template.last
        recStep(next, iteration + 1)
      }

    val fin = recStep(input._1, 0)
    val occurences = fin.groupMapReduce(identity)(_ => 1)(_ + _)
    val most = occurences.maxBy(_._2)
    val least = occurences.minBy(_._2)
    println(s"Most common is $most")
    println(s"Least common is $least")
    IO.pure(most._2 - least._2).map(_.toString)
  }

  override def parse2(strs: Vector[String]): IO[Input] = parse1(strs)

  override def solve2Impl(input: Input): IO[String] = ???
}
