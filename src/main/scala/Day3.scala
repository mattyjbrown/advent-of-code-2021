import cats.effect.IO

import scala.annotation.tailrec

object Day3 {
  object Part1 {

    def bitSolve: IO[Int] = Parser.parse(3).map { strs =>
      //Length checking
      val arrayOfStrings = strs.toArray
      assert(arrayOfStrings.nonEmpty)
      val bitLength = arrayOfStrings.head.length
      assert(arrayOfStrings.forall(_.length == bitLength))
      val arrayOfInts = arrayOfStrings.map(Integer.parseInt(_, 2))

      //Statics
      val midpoint = (arrayOfInts.length + 1) / 2
      val intLength = 32
      val topBitMask = 1 << (intLength - 1) //100000000000...

      //Shift everything over so we only consider top bits.
      //This also means we can throw away top bits by left shifting
      //And we can just sort the array by the Int because big endian (or is it little?)
      val uselessBits = intLength - bitLength
      arrayOfInts.mapInPlace(_ << uselessBits)

      //Sort the array, get the midpoint value, check the top bit with the mask.
      //Append the midpoint value, left shift EVERYTHING by 1 to look at the next bit, repeat.
      var gamma = 0
      for (_ <- 0 until bitLength) {
        gamma = gamma << 1
        arrayOfInts.sortInPlace()
        val mid = arrayOfInts(midpoint)
        val mostCommon = if ((mid & topBitMask) == topBitMask) 1 else 0
        gamma = gamma + mostCommon
        arrayOfInts.mapInPlace(_ << 1)
      }

      //Bitwise invert for epsilon, but only keep the bottom bits
      val bitLengthMask = (1 << bitLength) - 1 //...000000011111
      val epsilon = (~ gamma) & bitLengthMask
      gamma * epsilon
    }

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
