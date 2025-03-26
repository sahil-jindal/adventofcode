package day06

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def parseInput(input: List[String]): List[List[Set[Char]]] = groupLines(input).map(_.map(_.toSet))

def evaluatorOne(input: List[List[Set[Char]]]): Int = input.map(_.reduce(_ ++ _).size).sum
def evaluatorTwo(input: List[List[Set[Char]]]): Int = input.map(_.reduce(_ & _).size).sum

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day06.txt") match {
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