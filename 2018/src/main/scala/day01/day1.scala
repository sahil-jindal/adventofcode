package day01

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Set

def parseInput(lines: List[String]): List[Int] = lines.map(_.toInt)

def evaluatorOne(numbers: List[Int]): Int = numbers.sum

def evaluatorTwo(numbers: List[Int]): Int = {
    val seen = Set.empty[Int]
    return Iterator.continually(numbers).flatten.scanLeft(0)(_ + _).find(f => !seen.add(f)).get
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day01.txt") match {
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