package day01

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: String) = input.map(ch => {
    ch match {
        case '(' => 1
        case ')' => -1
        case _ => 0
    }
})

def evaluatorOne(input: IndexedSeq[Int]): Int = input.sum
def evaluatorTwo(input: IndexedSeq[Int]): Int = input.scanLeft(0)(_ + _).indexOf(-1)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day01.txt") match {
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