package day11

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

type Cache = Map[(String, Int), Long]

def eval(n: Long, blinks: Int, cache: Cache): Long = {
    val key = (n.toString, blinks)

    return cache.getOrElseUpdate(key, key match {
        case (_, 0) => 1L
        case ("0", _) => eval(1, blinks - 1, cache)
        case (str, _) if str.length % 2 == 0 => {
            val mid = str.length / 2
            val left = str.substring(0, mid).toLong
            val right = str.substring(mid).toLong
            eval(left, blinks - 1, cache) + eval(right, blinks - 1, cache)
        }
        case _ => eval(2024 * n, blinks - 1, cache)
    })
}

def stoneCount(input: String, blinks: Int): Long = {
    return input.split(" ").map(n => eval(n.toLong, blinks, Map.empty)).sum
}

def evaluatorOne(input: String): Long = stoneCount(input, 25)
def evaluatorTwo(input: String): Long = stoneCount(input, 75)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day11.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines.head)}")
            println(s"Part Two: ${evaluatorTwo(lines.head)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}