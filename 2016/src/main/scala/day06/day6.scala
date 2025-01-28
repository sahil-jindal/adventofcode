package day06

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def groupAlphabetsByFrequency(input: List[String]) = 
    input.transpose.map(_.groupBy(identity).view.mapValues(_.size))

def evaluatorOne(input: List[String]) =
    groupAlphabetsByFrequency(input).map(_.maxBy(_._2)._1).mkString

def evaluatorTwo(input: List[String]) =
    groupAlphabetsByFrequency(input).map(_.minBy(_._2)._1).mkString

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("day06.txt") match
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }