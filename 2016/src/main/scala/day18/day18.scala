package day18

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: String): List[Boolean] = input.map(_ == '^').toList

def safeCount(input: String, row: Int): Int = {
    var current = parseInput(input)
    var count = 0

    for _ <- 1 to row do {
        count += current.count(!_)
        val temp = List(false) ++ current ++ List(false)
        current = temp.sliding(3).map { it => it(0) ^ it(2) }.toList
    }

    return count
}

def evaluatorOne(input: String): Int = safeCount(input, 40)
def evaluatorTwo(input: String): Int = safeCount(input, 400000)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("day18.txt") match
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines.head)}")
            println(s"Part Two: ${evaluatorTwo(lines.head)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }