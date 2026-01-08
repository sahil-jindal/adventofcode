package day18

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: String) = input.collect {
    case '^' => true
    case '.' => false
}

def safeCount(input: IndexedSeq[Boolean], row: Int): Int = {
    var current = input
    var count = 0

    for (_ <- 1 to row) {
        count += current.count(!_)
        val temp = false +: current :+ false
        current = temp.sliding(3).map { it => it(0) ^ it(2) }.toIndexedSeq
    }

    return count
}

def evaluatorOne(input: IndexedSeq[Boolean]): Int = safeCount(input, 40)
def evaluatorTwo(input: IndexedSeq[Boolean]): Int = safeCount(input, 400000)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day18.txt") match {
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