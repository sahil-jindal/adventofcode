package day02

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(lines: List[String]): List[List[Int]] = {
    return lines.map("(\\d+)".r.findAllIn(_).map(_.toInt).toList)
}

def evaluatorOne(input: List[List[Int]]): Int = input.map { it => it.max - it.min }.sum

def evaluatorTwo(input: List[List[Int]]): Int = input.flatMap { numbers =>
    for { a <- numbers; b <- numbers; if a > b && a % b == 0 } yield a / b
}.sum

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day02.txt") match {
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