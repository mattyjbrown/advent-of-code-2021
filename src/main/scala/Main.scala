import cats.effect.{IO, IOApp}

object Main extends IOApp.Simple {
  override def run: IO[Unit] =
    for {
      _ <- IO.println("Hello, World!")
      pt1 <- Day1.solve1
      _ <- IO.println(s"Day 1, Part 1: $pt1")
      pt2 <- Day1.solve2
      _ <- IO.println(s"Day 1, Part 2: $pt2")
    } yield ()
}
