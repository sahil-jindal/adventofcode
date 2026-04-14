package day04

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Range(start: Int, end: Int) {
    def contains(r2: Range) = start <= r2.start && r2.end <= end
    def overlaps(r2: Range) = r2.start <= end && start <= r2.end
}

type Pair = (Range, Range)

def parseInput(input: List[String]) = input.map(line => {
    val Seq(sf, ef, ss, es) = raw"(\d+)".r.findAllIn(line).map(_.toInt).toSeq
    (Range(sf, ef), Range(ss, es))
})

def partOne(r1: Range, r2: Range) = r1.contains(r2) || r2.contains(r1)
def partTwo(r1: Range, r2: Range) = r1.overlaps(r2)

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