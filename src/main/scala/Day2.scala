import cats.effect.IO
import cats.implicits._

object Day2 extends Day {
  sealed trait Command

  object Command {
    case class Forward(units: Int) extends Command

    case class Up(units: Int) extends Command

    case class Down(units: Int) extends Command

    def parse(string: String): IO[Command] = {
      (for {
        units <- string.last.toString.toIntOption
        command <- string.takeWhile(_ != ' ') match {
          case "forward" => Forward(units).some
          case "up" => Up(units).some
          case "down" => Down(units).some
          case _ => None
        }
      } yield command).liftTo[IO](new IllegalArgumentException(s"Couldn't parse $string"))
    }
  }
  object Part1 {
    case class Location(x: Int, y: Int) {
      def move(command: Command): Location = command match {
        case Command.Forward(units) => copy(x = x + units)
        case Command.Up(units) => copy(y = y - units)
        case Command.Down(units) => copy(y = y + units)
      }
    }

    def solve(strs: Vector[String]): IO[Int] = strs
      .traverse(Command.parse)
      .map(_.foldLeft(Location(0, 0))(_ move _))
      .map { case Location(x, y) => x * y }
  }

  object Part2 {
    case class Sub(aim: Int, x: Int, y: Int) {
      def move(command: Command): Sub = command match {
        case Command.Forward(units) => copy(x = x + units, y = y + (aim * units))
        case Command.Up(units) => copy(aim = aim - units)
        case Command.Down(units) => copy(aim = aim + units)
      }
    }
    def solve(strs: Vector[String]): IO[Int] = strs
      .traverse(Command.parse)
      .map(_.foldLeft(Sub(0, 0, 0))(_ move _))
      .map { case Sub(_, x, y) => x * y }
  }

  override def day: Int = 2

  override def solve1Impl(strs: Vector[String]): IO[String] = Part1.solve(strs).map(_.toString)

  override def solve2Impl(strs: Vector[String]): IO[String] = Part2.solve(strs).map(_.toString)
}
