package day04

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(lines: List[String]) = {
    lines.map(_.split(" ")).toArray
}

def isValidLineCount(input: Array[Array[String]], normalizer: String => String) = {
    input.count(it => {
        val normalized = it.map(normalizer)
        normalized.toSet.size == normalized.size
    })
}

def evaluatorOne(input: Array[Array[String]]) = isValidLineCount(input, identity)
def evaluatorTwo(input: Array[Array[String]]) = isValidLineCount(input, it => it.sorted)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("day04.txt") match
        case Success(lines) => {
            val input = parseInput(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
