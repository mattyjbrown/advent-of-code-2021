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

  /**
    * So just bumping part 1 to 40 steps OOMed after step 22 or so.
    * Would it help if we were to just consider a single pair at time?
    * Like if your first pair is NN, you can do that for 40 steps, throw away all the garbage, keep the counts
    * and move on to the next pair.
    * No luck, still only made it to step 27.
    *
    * What about if you take each pair and do 10 iterations on it?
    * Then put that in a map of pair -> count of resulting pairs.
    * Find every possible char pair in our input.
    * Do 10 iterations of each and count the number of resulting pairs
    *
    * Got so confused by that I got lost, no thanks.
    * Pair counting!
    * Count all the pairs in the initial string, each of them will be mapped by the config to either itself or a pair-of-pairs.
    * Because the new element always goes in the middle, you don't affect the other pairs.
    * So you either put into the new map the same number again, or double that number of the new pair
    */
  override def solve2Impl(input: Input): IO[String] = {
    val initialCounts: Map[String, Int] =
      input._1.sliding(2).toList.groupMapReduce(identity)(_ => 1)(_ + _)
    val insertRules: Map[String, (String, String)] = {
      val charMap = input._2.map(r => r.pair -> r.insert).toMap
      charMap.map { case ((a, b), out) => s"$a$b" -> (s"$a$out" -> s"$out$b") }
    }
    val insertRule: String => List[String] = pairString => ins
    val step: State[Map[String, Int], Unit] = State.modify { pairCounts =>
      pairCounts.keys
    }
  }
}
