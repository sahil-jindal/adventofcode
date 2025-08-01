package day25

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def parsePattern(input: List[String]) = input.transpose.map(_.count(_ == '#'))

def checkMatch(k: List[Int], l: List[Int]) = (k zip l).forall { case (a, b) => a + b <= 7 }

def solver(input: List[String]): Int = {
    val patterns = groupLines(input)

    val keys = patterns.filter(_.head.forall(_ == '.')).map(parsePattern)
    val locks = patterns.filter(_.head.forall(_ == '#')).map(parsePattern)

    return keys.map(k => locks.count(l => checkMatch(k, l))).sum
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day25.txt") match {
        case Success(lines) => println(s"Answer: ${solver(lines)}")
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}