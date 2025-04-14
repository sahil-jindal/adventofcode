package day04

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Range(from: Int, to: Int)
case class Pair(first: Range, second: Range)

val inputRegex = raw"(\d+)-(\d+),(\d+)-(\d+)".r

def parseInput(input: List[String]) = input.map(line => {
    val nums = inputRegex.findFirstMatchIn(line).get.subgroups.map(_.toInt)
    Pair(Range(nums(0), nums(1)), Range(nums(2), nums(3)))
})

def duplicateWorkCount(input: List[Pair], rangeCheck: (Range, Range) => Boolean): Int = {
    return input.count { case Pair(first, second) => rangeCheck(first, second) || rangeCheck(second, first) }
}

def contains(r1: Range, r2: Range): Boolean = r1.from <= r2.from && r2.to <= r1.to 
def overlaps(r1: Range, r2: Range): Boolean = r1.to >= r2.from && r1.from <= r2.to

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