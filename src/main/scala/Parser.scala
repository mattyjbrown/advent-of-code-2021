import cats.effect.{IO, Resource}

import scala.io.{BufferedSource, Source}

object Parser {
  def parse(day: Int): IO[Vector[String]] = {
    Resource.make[IO, BufferedSource](IO(Source.fromFile(s"src/main/resources/Day$day.txt")))(s => IO(s.close)).use(s => IO(s.getLines.toVector))
  }
}
