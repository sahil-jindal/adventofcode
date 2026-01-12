package day20

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Range(start: Long, end: Long)

def parseInput(input: List[String]) = input.collect {
    case s"$start-$end" => Range(start.toLong, end.toLong)
}

def mergeRanges(ranges: List[Range]): List[Range] = {
    if (ranges.size < 2) return ranges

    val sortedRanges = ranges.sortBy(_.start)
    val (current, remaining) = (sortedRanges.head, sortedRanges.tail)
    
    return remaining.foldLeft(List(current)) { (merged, current) =>
        val Range(lastStart, lastEnd) = merged.last
        val Range(currStart, currEnd) = current

        if (currStart <= lastEnd + 1) {
            merged.init :+ Range(lastStart, lastEnd max currEnd)
        } else {
            merged :+ current
        }
    }
}

def evaluatorOne(ranges: List[Range]): Long = {
    return (ranges.init zip ranges.tail).collectFirst { case (r1, r2) if r2.start - r1.end > 1 => r1.end + 1 }.get
}

def evaluatorTwo(ranges: List[Range]): Long = {
    return (ranges.init zip ranges.tail).collect { case (r1, r2) if r2.start - r1.end > 1 => r2.start - r1.end - 1 }.sum
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day20.txt") match {
        case Success(lines) => {
            val ranges = mergeRanges(parseInput(lines))
            println(s"Part One: ${evaluatorOne(ranges)}")
            println(s"Part Two: ${evaluatorTwo(ranges)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}