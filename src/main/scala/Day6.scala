import cats.data.State
import cats.effect.IO
import cats.implicits._

object Day6 extends Day[List[Int], List[Int]] {
  override def day: Int = 6

  override def parse1(strs: Vector[String]): IO[List[Int]] =
    IO(strs.head.split(",").map(_.toInt).toList)

  override def solve1Impl(input: List[Int]): IO[String] = {
    val step: State[List[Int], Unit] = State.modify { fishes =>
      val (nextFishes, newFishes) = fishes
        .map { fish =>
          val nextFish = fish - 1
          if (nextFish == -1) 6 -> Some(8)
          else nextFish -> None
        }
        .foldLeft(List.empty[Int] -> List.empty[Int]) {
          case ((nextFishes, newFishes), (nextFish, None)) =>
            (nextFish :: nextFishes) -> newFishes
          case ((nextFishes, newFishes), (nextFish, Some(newFish))) =>
            (nextFish :: nextFishes) -> (newFish :: newFishes)
        }
      nextFishes ::: newFishes
    }
    val days = 80
    IO(List.fill(days)(step).sequence.runS(input).value.size.toString)
  }

  override def parse2(strs: Vector[String]): IO[List[Int]] = parse1(strs)

  /**
    * OK, maintaining a list of all fish is impractical - but every new fish generated per step is identical
    * All fish are from 0 to 8.
    * Just keep a count of the number at each
    */
  override def solve2Impl(input: List[Int]): IO[String] = {
    case class FishCount(c0: Long,
                         c1: Long,
                         c2: Long,
                         c3: Long,
                         c4: Long,
                         c5: Long,
                         c6: Long,
                         c7: Long,
                         c8: Long) {
      def sum: Long = c0 + c1 + c2 + c3 + c4 + c5 + c6 + c7 + c8
    }
    val step: State[FishCount, Unit] = State.modify { fishes =>
      fishes.copy(
        c0 = fishes.c1,
        c1 = fishes.c2,
        c2 = fishes.c3,
        c3 = fishes.c4,
        c4 = fishes.c5,
        c5 = fishes.c6,
        c6 = fishes.c7 + fishes.c0,
        c7 = fishes.c8,
        c8 = fishes.c0
      )
    }
    val days = 256
    val initMap =
      input.groupMapReduce(identity)(_ => 1)(_ + _).withDefaultValue(0)
    val init = FishCount(
      initMap(0),
      initMap(1),
      initMap(2),
      initMap(3),
      initMap(4),
      initMap(5),
      initMap(6),
      initMap(7),
      initMap(8)
    )
    IO(List.fill(days)(step).sequence.runS(init).value.sum.toString)
  }
}
