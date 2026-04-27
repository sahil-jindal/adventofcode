package day04

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Inclusive(start: Int, end: Int) {
    def contains(r2: Inclusive) = start <= r2.start && r2.end <= end
    def overlaps(r2: Inclusive) = r2.start <= end && start <= r2.end
}

type Pair = (Inclusive, Inclusive)

def parseInput(input: List[String]) = input.map(line => {
    val Seq(sf, ef, ss, es) = raw"(\d+)".r.findAllIn(line).map(_.toInt).toSeq
    (Inclusive(sf, ef), Inclusive(ss, es))
})

def partOne(r1: Inclusive, r2: Inclusive) = r1.contains(r2) || r2.contains(r1)
def partTwo(r1: Inclusive, r2: Inclusive) = r1.overlaps(r2)

def evaluatorOne(input: List[Pair]): Int = input.count(partOne)
def evaluatorTwo(input: List[Pair]): Int = input.count(partTwo)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day04.txt") match {
        case Success(lines) => {
            val input = parseInput(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}