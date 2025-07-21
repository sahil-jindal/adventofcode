package day01

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: List[String]) = input.map(_.toInt)

def helper(weights: List[Int]) = weights.map(_ / 3 - 2).filter(_ > 0)

def evaluatorOne(weights: List[Int]): Int = helper(weights).sum

def evaluatorTwo(weights: List[Int]): Int = {
    return Iterator.iterate(weights)(helper).takeWhile(_.nonEmpty).drop(1).flatten.sum
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