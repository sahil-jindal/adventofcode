package day01

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def solve(line: Array[Int], rotated: Int) = {
    val d = rotated % line.length
    val leftRotated = line.drop(d) ++ line.take(d)
    val newLine = line zip leftRotated
    newLine.collect { case (a, b) if a == b => a }.sum
}

def evaluatorOne(line: Array[Int]) = solve(line, 1)
def evaluatorTwo(line: Array[Int]) = solve(line, line.length / 2)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("day01.txt") match
        case Success(lines) => {
            val input = lines(0).map(_.asDigit).toArray
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }