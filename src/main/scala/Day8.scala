import cats.effect.IO
import cats.implicits._
import Day8Utils._

object Day8Utils {
  val intMap: Map[String, Int] =
    Map(
      "ABCEFG" -> 0,
      "CF" -> 1,
      "ACDEG" -> 2,
      "ACDFG" -> 3,
      "BCDF" -> 4,
      "ABDFG" -> 5,
      "ABDEFG" -> 6,
      "ACF" -> 7,
      "ABCDEFG" -> 8,
      "ABCDFG" -> 9
    )

  case class PuzzleInput(inputs: List[String], outputs: List[String])

  implicit class SingleOps[A](traversable: Iterable[A]) {
    def single: A = traversable.toList match {
      case head :: Nil => head
      case other =>
        throw new IllegalArgumentException(s"$other is not length 1!")
    }
  }
}

object Day8 extends Day[Vector[PuzzleInput], Vector[PuzzleInput]] {
  override def day: Int = 8

  override def parse1(strs: Vector[String]): IO[Vector[PuzzleInput]] =
    IO {
      strs
        .map(s => s.splitAt(s.indexOf("|")))
        .map {
          case (input, output) =>
            PuzzleInput(
              input.toUpperCase.trim.split(' ').toList,
              output.toUpperCase.tail.trim.split(' ').toList
            )
        }
    }

  override def solve1Impl(input: Vector[PuzzleInput]): IO[String] = {
    IO(
      input
        .map(
          _.outputs.count(
            s =>
              s.length == 2 || s.length == 4 || s.length == 3 || s.length == 7
          )
        )
        .sum
        .toString
    )
  }

  override def parse2(strs: Vector[String]): IO[Vector[PuzzleInput]] =
    parse1(strs)

  override def solve2Impl(input: Vector[PuzzleInput]): IO[String] = {
    val all = "ABCDEFG".toSet
    input.map {
      case PuzzleInput(inputs, outputs) =>
        //What's the length histogram?
        //2 - 1
        //3 - 7
        //4 - 4
        //5 - 2, 3, 5
        //6 - 6, 9, 0
        //7 - 8

        //First, get the easy ones (by count) done
        //Length 2 - Must be 1, so these two are C and F
        //Length 4 - Must be 4, so these four are B and D and C and F
        //Length 3 - Must be 7, so these three are A and C and F
        val seven = inputs.find(_.length == 3).get.toSet
        val one = inputs.find(_.length == 2).get.toSet
        val four = inputs.find(_.length == 4).get.toSet
        val eight = inputs.find(_.length == 7).get.toSet

        //So we can work out that the one in 7 which is not in 1 must be A!
        val A = (seven diff one).single
        //Unfortunately A is in all the numbers except for 1 and 4, so that isn't much info.
        //What about 6,9,0?
        //6 is missing C, 9 is missing E, 0 is missing D
        //So get those three missing ones, and the one not in the known so far list is E
        val knowns: Set[Char] = four + A //A,B,C,D,F
        val sixNineZero = inputs.filter(_.length == 6).toSet
        val cde = sixNineZero.toList.flatten
          .groupMapReduce(identity)(_ => 1)(_ + _)
          .collect {
            case (c, 2) => c
          }
          .toSet
        val E = (cde diff knowns).single

        val sixAndNine = sixNineZero.filter(_.contains(E))
        println(knowns)
        val (nineSet, sixSet) =
          sixAndNine.partition(maybeSix => (one diff maybeSix.toSet).isEmpty)
        val six = sixSet.single
        val C = (all diff six.toSet).single
        val nine = nineSet.single

        // So from there we KNOW:
        // The letter in 7 but not in 1 is A
        // Identify the 6, the missing letter is C
        // Identify the 9, the missing letter is E
        // The letter we don't know in 7 is F
        val F = (seven - A - C).single

        //3 has A,C,D,F,G.
        //5 has A,B,D,F,G.
        //2 has A,C,D,E,G.
        // Considering the 5 length strings, they all share 3 - A,D,G. We know A, and D is one of our pairs from 4, so identify G!
        val bANDd = four diff one
        val fiveLengths = inputs.filter(_.length == 5)
        val shared = fiveLengths.reduce(_ intersect _).toSet
        println(shared)
        println(bANDd)
        val G = ((shared diff bANDd) - A).single
        // The one which contains our pair B-D is 5.
        val five = fiveLengths.find(s => (s.toSet diff bANDd).size == 3).get
        //so left is just 2 and 3 | B and D
        //B only appears once in the five-lengths (so does E, but we know E!)
        //So if you do five minus the other two youre left with B (could just minus 3 but we don't know which one that is)
        val twoANDthree = fiveLengths.toSet - five
        val B = twoANDthree.foldLeft(five)(_ diff _).toSet.single
        val D = (bANDd - B).single

        //We have all our letters! We want to decode our output, so:
        val decodeMap = Map(
          A -> 'A',
          B -> 'B',
          C -> 'C',
          D -> 'D',
          E -> 'E',
          F -> 'F',
          G -> 'G'
        )
        println(decodeMap)
        val decodedOutputs = outputs.map { out =>
          val decoded = out.map(decodeMap).sorted
          println(s"$out decodes to $decoded")
          intMap(decoded)
        }
        val digits = decodedOutputs.mkString
        digits.toInt
    }
  }.sum.toString.pure[IO]
}
