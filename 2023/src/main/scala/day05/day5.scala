package day05

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, ListBuffer}

case class Range(begin: Long, end: Long) {
    def contains(that: Range) = begin <= that.begin && that.end <= end
    def overlaps(that: Range) = begin <= that.end && that.begin <= end 
}

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def parseNumbers(input: String): List[Long] = {
    return raw"\d+".r.findAllIn(input).map(_.toLong).toList
}

def parseMap(input: List[String]): Map[Range, Range] = {
    return input.tail.map(line => {
        val List(a, b, c) = parseNumbers(line)
        Range(b, c + b - 1) -> Range(a, c + a - 1)
    }).toMap
}

def project(inputRanges: List[Range], map: Map[Range, Range]): List[Range] = {
    val input = Queue(inputRanges*)
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
                val shift = dst.begin - src.begin
                output += Range(range.begin + shift, range.end + shift)
            } else if (range.begin < src.begin) {
                input.enqueue(Range(range.begin, src.begin - 1))
                input.enqueue(Range(src.begin, range.end))
            } else {
                input.enqueue(Range(range.begin, src.end))
                input.enqueue(Range(src.end + 1, range.end))
            }
        }
    }

    return output.toList
}

def partOneRanges(numbers: List[Long]): List[Range] = numbers.map(n => Range(n, n))

def partTwoRanges(numbers: List[Long]): List[Range] = {
    return numbers.grouped(2).map(n => Range(n(0), n(0) + n(1) - 1)).toList
}

def solve(input: List[String], parseSeeds: List[Long] => List[Range]): Long = {
    val blocks = groupLines(input)
    val seedRanges = parseSeeds(parseNumbers(blocks(0)(0)))
    val maps = blocks.tail.map(parseMap)

    return maps.foldLeft(seedRanges)(project).map(_.begin).min
}

def evaluatorOne(input: List[String]): Long = solve(input, partOneRanges)
def evaluatorTwo(input: List[String]): Long = solve(input, partTwoRanges)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day05.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}