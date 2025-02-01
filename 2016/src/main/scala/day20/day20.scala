package day20

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Range(val start: Long, val end: Long)

def parseInput(line: String): Range = {
    val Array(start, end) = line.split("-")
    Range(start.toLong, end.toLong)
}

def mergeRanges(ranges: List[Range]): List[Range] = {
    if (ranges.isEmpty) return Nil

    val sortedRanges = ranges.sortBy(_.start)
    
    sortedRanges.tail.foldLeft(List(sortedRanges.head)) { (merged, current) =>
        val Range(lastStart, lastEnd) = merged.last
        val Range(currStart, currEnd) = current

        if (currStart <= lastEnd + 1) then
            merged.init :+ Range(lastStart, lastEnd max currEnd)
        else
            merged :+ current
    }
}

def evaluatorOne(ranges: List[Range]) = 
    mergeRanges(ranges).sliding(2).collectFirst {
        case List(r1, r2) if r2.start - r1.end > 1 => r1.end + 1 
    }.get

def evaluatorTwo(ranges: List[Range]) = 
    mergeRanges(ranges).sliding(2).collect {
        case List(r1, r2) if r2.start - r1.end > 1 => r2.start - r1.end - 1 
    }.sum

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("day20.txt") match
        case Success(lines) => {
            val ranges = lines.map(parseInput)
            println(s"Part One: ${evaluatorOne(ranges)}")
            println(s"Part Two: ${evaluatorTwo(ranges)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }