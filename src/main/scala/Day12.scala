import Day12Utils._
import cats.effect.IO

object Day12Utils {
  case class Cave(name: String, next: List[String]) {
    val big: Boolean = name.forall(_.isUpper)
  }

  sealed trait VisitedState {

    /**
      * If returns None, not allowed
      */
    def tryVisit(name: String): Option[VisitedState] =
      if (name == "start") None
      else if (name.forall(_.isUpper)) Some(this)
      else
        this match {
          case VisitedOneTwice(seen) =>
            if (seen(name)) None
            else Some(VisitedOneTwice(seen + name))

          case NotVistedOneTwice(seen) =>
            if (seen(name)) Some(VisitedOneTwice(seen))
            else Some(NotVistedOneTwice(seen + name))
        }
  }
  case class VisitedOneTwice(seen: Set[String]) extends VisitedState
  case class NotVistedOneTwice(seen: Set[String]) extends VisitedState
}

object Day12 extends Day[List[Cave], List[Cave]] {
  override def day: Int = 12

  override def parse1(strs: Vector[String]): IO[List[Cave]] = {
    val pairs = strs.toList.map { next =>
      val (from, toTmp) = next.splitAt(next.indexOf("-"))
      val to = toTmp.tail
      from -> to
    }
    val (a, b) = pairs.unzip
    val all = a ++ b
    val fin = all.map { c =>
      c -> pairs.collect {
        case (f, t) if t == c => f
        case (f, t) if f == c => t
      }
    }.toMap
    IO.pure(fin.map(Cave.tupled).toList)
  }

  override def solve1Impl(input: List[Cave]): IO[String] = {
    val asMap = input.groupMapReduce(_.name)(identity)((a, _) => a)
    assert(asMap.size == input.size)
    val start = asMap("start")

    def rec(at: Cave,
            path: List[String],
            seen: Set[String]): List[List[String]] =
      at.next.flatMap { link =>
        if (seen(link)) None //Dead end
        else if (link == "end") Some(link :: path) //The end!
        else {
          val thisCave = asMap(link)
          rec(thisCave, link :: path, if (thisCave.big) seen else seen + link)
        }
      }

    val paths = rec(start, List("start"), Set("start"))
    println(s"${paths.size} paths:")
    paths.map(_.reverse).foreach(println)
    IO.pure(paths.size.toString)
  }

  override def parse2(strs: Vector[String]): IO[List[Cave]] = parse1(strs)

  override def solve2Impl(input: List[Cave]): IO[String] = {
    val asMap = input.groupMapReduce(_.name)(identity)((a, _) => a)
    assert(asMap.size == input.size)
    val start = asMap("start")

    def rec(at: Cave,
            path: List[String],
            seen: VisitedState): List[List[String]] =
      at.next.flatMap { link =>
        seen.tryVisit(link) match {
          case Some(nextSeen) =>
            if (link == "end") Some(link :: at.name :: path)
            else {
              val thisCave = asMap(link)
              rec(thisCave, at.name :: path, nextSeen)
            }
          case None => None
        }
      }

    val paths = start.next.flatMap(
      c => rec(asMap(c), List("start"), NotVistedOneTwice(Set(asMap(c).name)))
    )
    println(s"${paths.size} paths:")
    paths.map(_.reverse.mkString(",")).foreach(println)
    IO.pure(paths.size.toString)
  }
}
