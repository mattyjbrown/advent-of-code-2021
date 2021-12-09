import cats.effect.{IO, IOApp}

object Main extends IOApp.Simple {
  override def run: IO[Unit] = {
    val day = Day9
    for {
      _ <- IO.println("Hello, World!")
      pt1 <- day.solve1
      _ <- IO.println(s"Day ${day.day}, Part 1: $pt1")
      pt2 <- day.solve2
      _ <- IO.println(s"Day ${day.day}, Part 2: $pt2")
    } yield ()
  }
}

trait Day[T1, T2] {
  def day: Int
  def solve1: IO[String] =
    FileReader.read(day).flatMap(parse1).flatMap(solve1Impl)
  def parse1(strs: Vector[String]): IO[T1]
  def solve1Impl(input: T1): IO[String]
  def solve2: IO[String] =
    FileReader.read(day).flatMap(parse2).flatMap(solve2Impl)
  def parse2(strs: Vector[String]): IO[T2]
  def solve2Impl(input: T2): IO[String]
}
