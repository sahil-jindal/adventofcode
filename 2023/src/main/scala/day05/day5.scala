package day05

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, ListBuffer}

case class Range(start: Long, end: Long) {
    def contains(that: Range) = start <= that.start && that.end <= end
    def overlaps(that: Range) = start <= that.end && that.start <= end 
}

case class Pair(numbers: List[Long], maps: List[Map[Range, Range]])

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def parseNumbers(input: String) = raw"(\d+)".r.findAllIn(input).map(_.toLong).toList

def parseInput(input: List[String]): Pair = {
    val maps = groupLines(input.drop(2)).map(_.tail.map(line => {
        val List(sA, sB, len) = parseNumbers(line)
        Range(sB, sB + len - 1) -> Range(sA, sA + len - 1)
    }).toMap)

    return Pair(parseNumbers(input.head), maps)
}

def project(inputRanges: List[Range], map: Map[Range, Range]): List[Range] = {
    val input = Queue.from(inputRanges)
    val output = ListBuffer.empty[Range]

    while (input.nonEmpty) {
        val range = input.dequeue()
        // If no entry intersects our range -> just add it to the output. 
        // If an entry completely contains the range -> add after mapping.
        // Otherwise, some entry partly covers the range. In this case 'chop' 
        // the range into two halfs getting rid of the intersection. The new 
        // pieces are added back to the queue for further processing and will be 
        // ultimately consumed by the first two cases.

        val found = map.keys.find(_.overlaps(range))

        if (found.isEmpty) {
            output += range
        } else {
            val src = found.get
            if (src.contains(range)) {
                val dst = map(src)
                val shift = dst.start - src.start
                output += Range(range.start + shift, range.end + shift)
            } else if (range.start < src.start) {
                input.enqueue(Range(range.start, src.start - 1))
                input.enqueue(Range(src.start, range.end))
            } else {
                input.enqueue(Range(range.start, src.end))
                input.enqueue(Range(src.end + 1, range.end))
            }
        }
    }

    return output.toList
}

def solve(input: Pair, parseSeeds: List[Long] => List[Range]): Long = {
    val seedRanges = parseSeeds(input.numbers)
    return input.maps.foldLeft(seedRanges)(project).map(_.start).min
}

def partOneRanges(numbers: List[Long]): List[Range] = numbers.map(n => Range(n, n))

def partTwoRanges(numbers: List[Long]): List[Range] = {
    return numbers.grouped(2).map(n => Range(n(0), n(0) + n(1) - 1)).toList
}

def evaluatorOne(input: Pair): Long = solve(input, partOneRanges)
def evaluatorTwo(input: Pair): Long = solve(input, partTwoRanges)

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