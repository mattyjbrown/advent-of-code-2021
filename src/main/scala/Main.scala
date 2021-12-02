import cats.effect.{IO, IOApp}

object Main extends IOApp.Simple {
  override def run: IO[Unit] =
    for {
      _ <- IO.println("Hello, World!")
      pt1 <- Day2.Part1.solve
      _ <- IO.println(s"Day 2, Part 1: $pt1")
      pt2 <- Day2.Part2.solve
      _ <- IO.println(s"Day 2, Part 2: $pt2")
    } yield ()
}
