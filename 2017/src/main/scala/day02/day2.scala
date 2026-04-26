package day02

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: List[String]) = input.map(line => {
    raw"(\d+)".r.findAllIn(line).map(_.toInt).toVector.sorted
})

def evaluatorOne(input: List[Vector[Int]]): Int = {
    return input.map(it => it.last - it.head).sum
}

def evaluatorTwo(input: List[Vector[Int]]): Int = {
    return input.flatMap(values => {
        (for { 
            smaller <- values
            larger <- values.reverseIterator.takeWhile(_ >= 2*smaller)
            if larger % smaller == 0 
        } yield larger / smaller)
    }).sum
}

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