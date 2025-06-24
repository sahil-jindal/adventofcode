package day09

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer

def parseInput(input: List[String]): List[List[Int]] = { 
    return input.map(_.split(" ").map(_.toInt).toList)
}

def extrapolateRight(numbers: List[Int]): Int = {
    var current = numbers
    val lastValues = ListBuffer(current.last)

    while (current.exists(_ != 0)) {
        current = (current.init zip current.tail).map { case (a, b) => b - a }
        lastValues += current.last
    }

    return lastValues.sum
}

def extrapolateLeft(numbers: List[Int]): Int = extrapolateRight(numbers.reverse)

def evaluatorOne(input: List[List[Int]]): Int = input.map(extrapolateRight).sum
def evaluatorTwo(input: List[List[Int]]): Int = input.map(extrapolateLeft).sum

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day09.txt") match {
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