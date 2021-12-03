import cats.effect.IO

import scala.annotation.tailrec

object Day3 {
  object Part1 {
    def solve: IO[Int] = Parser.parse(3).map { strs =>
      //Gamma - most common bit at each location
      //Epsilon - least common bit at each location
      //Power consumption gamma * epsilon, in decimal

      //They're always going to be inverse, right?
      //So just count the number of 1s in each position.
      val gammaStr = strs
        .transpose //Transposing gives us a list where each element is a list which is all the chars in each position
        .map(_.count(_ == '0') > strs.length / 2) //Each element is now "is the number of 0s more than half
        .map(if (_) '0' else '1')
        .mkString

      println(gammaStr)
      val gamma = Integer.parseInt(gammaStr, 2)
      println(gamma)
      val epsilonStr = gammaStr.replace('1', '2').replace('0', '1').replace('2', '0')
      println(epsilonStr)
      val epsilon = Integer.parseInt(epsilonStr, 2)
      println(epsilon)
      gamma * epsilon
    }
  }
  object Part2 {
    sealed trait MostOrLeast
    case object Most extends MostOrLeast
    case object Least extends MostOrLeast
    def solve: IO[Int] = Parser.parse(3).map { strs =>
      @tailrec
      def rec(remaining: Vector[String], iteration: Int, mostOrLeast: MostOrLeast): Vector[String] = {
        val transposed = remaining.transpose
        val matcher: Char = {
          val zeros = transposed(iteration).count(_ == '0')
          mostOrLeast match {
            case Most => if (zeros > remaining.length / 2) '0' else '1'
            case Least => if (zeros > remaining.length / 2) '1' else '0'
          }
        }
        val left = remaining.filter(_(iteration) == matcher)
        println(s"$left  -  $iteration  -  $mostOrLeast  -  $matcher")
        if (left.size <= 1) left else rec(left, iteration + 1, mostOrLeast)
      }

      val oxygen = rec(strs, 0, Most)
      val co2 = rec(strs, 0, Least)
      (oxygen, co2) match {
        case (Vector(ox), Vector(co)) =>
          Integer.parseInt(ox, 2) * Integer.parseInt(co, 2)
        case _ => ???
      }
    }
  }
}
