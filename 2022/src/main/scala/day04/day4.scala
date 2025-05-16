package day04

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Range(start: Int, end: Int)
case class Pair(first: Range, second: Range)

def parseInput(input: List[String]) = input.map(line => {
    val Seq(sf, ef, ss, es) = raw"(\d+)".r.findAllIn(line).map(_.toInt).toSeq
    Pair(Range(sf, ef), Range(ss, es))
})

def duplicateWorkCount(input: List[Pair], rangeCheck: (Range, Range) => Boolean): Int = {
    return input.count { case Pair(first, second) => rangeCheck(first, second) || rangeCheck(second, first) }
}

def contains(r1: Range, r2: Range): Boolean = r1.start <= r2.start && r2.end <= r1.end 
def overlaps(r1: Range, r2: Range): Boolean = r2.start <= r1.end && r1.start <= r2.end

def evaluatorOne(input: List[Pair]): Int = duplicateWorkCount(input, contains)
def evaluatorTwo(input: List[Pair]): Int = duplicateWorkCount(input, overlaps)

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