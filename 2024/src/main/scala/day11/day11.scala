package day11

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

type Cache = Map[(String, Int), Long]

def parseInput(line: String) = raw"(\d+)".r.findAllIn(line).map(_.toLong).toList

def eval(n: Long, blinks: Int, cache: Cache): Long = {
    val key = (n.toString, blinks)

    return cache.getOrElseUpdate(key, key match {
        case (_, 0) => 1L
        case ("0", _) => eval(1, blinks - 1, cache)
        case (str, _) if str.length % 2 == 0 => {
            val mid = str.length / 2
            val (left, right) = str.splitAt(mid)
            eval(left.toLong, blinks - 1, cache) + eval(right.toLong, blinks - 1, cache)
        }
        case _ => eval(2024 * n, blinks - 1, cache)
    })
}

def evaluatorOne(input: List[Long]): Long = input.map(eval(_, 25, Map.empty)).sum
def evaluatorTwo(input: List[Long]): Long = input.map(eval(_, 75, Map.empty)).sum

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day11.txt") match {
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