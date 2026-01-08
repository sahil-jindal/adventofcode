package day05

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Range(start: Long, end: Long) {
    def length = end - start + 1
}

case class Data(ranges: List[Range], ids: List[Long])

def parseInput(lines: List[String]): Data = {
    val idx = lines.indexWhere(_.trim.isEmpty)

    val ranges = lines.take(idx).collect {
        case s"$start-$end" => Range(start.toLong, end.toLong)
    }

    val numbers = lines.drop(idx + 1).map(_.toLong)

    Data(ranges, numbers)
}

def evaluatorOne(data: Data): Int = {
    val Data(ranges, ids) = data
    return ids.count(id => ranges.exists(r => id >= r.start && id <= r.end))
}

def evaluatorTwo(data: Data): Long = {
    val ranges = data.ranges

    if (ranges.isEmpty) return 0
    if (ranges.size == 1) return ranges.head.length

    val sortedRanges = ranges.sortBy(_.start)
    val (current, remaining) = (sortedRanges.head, sortedRanges.tail)
    
    val mergedRanges = remaining.foldLeft(List(current)) { (merged, current) =>
        val Range(lastStart, lastEnd) = merged.last
        val Range(currStart, currEnd) = current

        if (currStart <= lastEnd + 1) {
            merged.init :+ Range(lastStart, lastEnd max currEnd)
        } else {
            merged :+ current
        }
    }

    return mergedRanges.map(_.length).sum
}

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