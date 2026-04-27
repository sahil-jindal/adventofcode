package day05

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Inclusive(start: Long, end: Long) {
    def length = end - start + 1
}

case class Input(ranges: List[Inclusive], ids: List[Long])

def mergeRanges(ranges: List[Inclusive]): List[Inclusive] = {
    if (ranges.size <= 1) return ranges

    val sortedRanges = ranges.sortBy(_.start)
    val (current, remaining) = (sortedRanges.head, sortedRanges.tail)
    
    return remaining.foldLeft(List(current)) { (merged, current) =>
        val Inclusive(lastStart, lastEnd) = merged.last
        val Inclusive(currStart, currEnd) = current

        if (currStart <= lastEnd + 1) {
            merged.init :+ Inclusive(lastStart, lastEnd max currEnd)
        } else {
            merged :+ current
        }
    }
}

def parseInput(lines: List[String]): Input = {
    val idx = lines.indexWhere(_.trim.isEmpty)

    val ranges = lines.take(idx).collect {
        case s"$start-$end" => Inclusive(start.toLong, end.toLong)
    }

    val numbers = lines.drop(idx + 1).map(_.toLong)

    return Input(mergeRanges(ranges), numbers)
}

def evaluatorOne(data: Input): Int = {
    val Input(ranges, ids) = data
    return ids.count(id => ranges.exists(r => id >= r.start && id <= r.end))
}

def evaluatorTwo(data: Input): Long = data.ranges.map(_.length).sum

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day05.txt") match {
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