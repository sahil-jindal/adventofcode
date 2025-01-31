package day18

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

def parseInput(input: String): IndexedSeq[Boolean] = {
    input.map { it => if it == '^' then true else false }
}

def safeCount(input: String, row: Int): Int = {
    var current = parseInput(input)
    var count = 0

    for _ <- 1 to row do {
        count += current.count(!_)
        val temp = IndexedSeq(false) ++ current ++ IndexedSeq(false)
        current = temp.sliding(3).map { it => it(0) ^ it(2) }.toIndexedSeq
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
            println(s"Part One: ${evaluatorOne(lines(0))}")
            println(s"Part Two: ${evaluatorTwo(lines(0))}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }