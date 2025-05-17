package day03

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.matching.Regex

case class Part(text: String, y: Int, x: Int) {
    def toInt = text.toInt
}

def parse(input: List[String], rx: Regex): List[Part] = {
    return (for {
        (line, y) <- input.zipWithIndex
        matchData <- rx.findAllMatchIn(line)
    } yield Part(matchData.matched, y, matchData.start))
}

// checks that the parts are touching each other, i.e. rows are within 1 
// step and also the columns (using https://stackoverflow.com/a/3269471).
def adjacent(p1: Part, p2: Part): Boolean = {
    (p2.y - p1.y).abs <= 1 &&
    p1.x <= p2.x + p2.text.length &&
    p2.x <= p1.x + p1.text.length
}

def evaluatorOne(input: List[String]): Int = {
    val symbols = parse(input, raw"([^.0-9])".r)
    val numbers = parse(input, raw"(\d+)".r)

    return numbers.collect { case n if symbols.exists(s => adjacent(s, n)) => n.toInt }.sum
}

def evaluatorTwo(input: List[String]): Int = {
    val gears = parse(input, raw"(\*)".r)
    val numbers = parse(input, raw"(\d+)".r)

    return (for {
        g <- gears
        neighbours = numbers.collect { case n if adjacent(n, g) => n.toInt }
        if neighbours.size == 2
    } yield neighbours(0) * neighbours(1)).sum
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day03.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}