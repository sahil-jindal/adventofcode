package day04

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: List[String]) = input.map(_.split(" ").toList)

def isValidLineCount(input: List[List[String]], normalizer: String => String): Int = {
    return input.map(_.map(normalizer)).count(it => it.toSet.size == it.size)
}

def evaluatorOne(input: List[List[String]]): Int = isValidLineCount(input, identity)
def evaluatorTwo(input: List[List[String]]): Int = isValidLineCount(input, it => it.sorted)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day04.txt") match {
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