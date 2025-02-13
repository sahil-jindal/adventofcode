package day06

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def evaluatorOne(input: List[List[Char]]): String = {
    return input.map(s => s.distinct.maxBy(c => s.count(_ == c))).mkString
}

def evaluatorTwo(input: List[List[Char]]): String = {
    return input.map(s => s.distinct.minBy(c => s.count(_ == c))).mkString
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day06.txt") match {
        case Success(lines) => {
            val actualInput = lines.transpose
            println(s"Part One: ${evaluatorOne(actualInput)}")
            println(s"Part Two: ${evaluatorTwo(actualInput)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}