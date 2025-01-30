package day15

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.annotation.tailrec

val pattern = """Disc #\d+ has (\d+) positions; at time=0, it is at position (\d+).""".r

case class Disc(val pos: Int, val mod: Int)

def parse(input: List[String]) = input.collect {
    case pattern(mod, pos) => Disc(pos.toInt, mod.toInt)
}

def iterate(discs: List[Disc]): LazyList[(Int, Boolean)] =
    LazyList.from(0).map { t =>
        val ok = discs.zipWithIndex.forall { case (disc, i) => (disc.pos + t + i + 1) % disc.mod == 0 }
        (t, ok)
    }

def evaluator(discs: List[Disc]): Int = 
    iterate(discs).collectFirst { case (t, true) => t }.get

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("day15.txt") match
        case Success(lines) => {
            val discs = parse(lines)
            println(s"Part One: ${evaluator(discs)}")

            val newDiscs = discs :+ Disc(0, 11)
            println(s"Part Two: ${evaluator(newDiscs)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }


