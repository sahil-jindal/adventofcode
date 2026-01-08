package day02

import scala.collection.immutable.NumericRange.Inclusive
import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.matching.Regex

val partOne = raw"(\d+)\1".r
val partTwo = raw"(\d+?)\1+".r

def parseInput(input: String) = input.split(',').collect {
    case s"$start-$end" => start.toLong to end.toLong
}

def helper(ranges: Array[Inclusive[Long]], pattern: Regex): Long = {
    return ranges.flatMap(_.filter(it => pattern.matches(it.toString))).sum
}

def evaluatorOne(ranges: Array[Inclusive[Long]]) = helper(ranges, partOne)
def evaluatorTwo(ranges: Array[Inclusive[Long]]) = helper(ranges, partTwo)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day02.txt") match {
        case Success(lines) => {
            val input = parseInput(lines.head)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}