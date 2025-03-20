package day01

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(lines: List[String]): List[Int] = lines.map(_.toInt)

def helper(weights: List[Int]): List[Int] = weights.map(_ / 3 - 2).filter(_ > 0)

def evaluatorOne(weights: List[Int]): Int = helper(weights).sum

def evaluatorTwo(weights: List[Int]): Int = {
    var fuels = helper(weights)
    var total = 0

    while fuels.nonEmpty do {
        total += fuels.sum
        fuels = helper(fuels)
    }

    return total
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