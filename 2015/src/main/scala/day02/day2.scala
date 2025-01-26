package day02

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

val dimensionRegex = raw"(\d+)x(\d+)x(\d+)".r

def getDimensions(line: String) = {
    dimensionRegex.findFirstMatchIn(line) match {
        case Some(m) =>
            val dimensions = List(
                m.group(1).toInt, m.group(2).toInt, m.group(3).toInt
            ).sorted
            (dimensions(0), dimensions(1), dimensions(2))
        case None =>
            throw new IllegalArgumentException(s"Invalid input: $line")
    }
}

def evaluatorOne(dimensions: (Int, Int, Int)) = {
    val (a, b, c) = dimensions
    3*a*b + 2*a*c + 2*b*c
}

def evaluatorTwo(dimensions: (Int, Int, Int)) = {
    val (a, b, c) = dimensions
    2*(a + b) + a*b*c
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("day02.txt") match
        case Success(lines) => {
            val dimensions = lines.map(getDimensions)
            println(s"Part One: ${dimensions.map(evaluatorOne).sum}")
            println(s"Part One: ${dimensions.map(evaluatorTwo).sum}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }