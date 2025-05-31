package day02

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def checkLine(input: String, n: Int): Boolean = {
    return input.groupBy(identity).values.map(_.length).exists(_ == n)
}

def diff(line1: String, line2: String): Int = {
    return (line1 zip line2).count { case (a, b) => a != b }
}

def common(line1: String, line2: String): String = {
    return (line1 zip line2).collect { case (a, b) if a == b => a }.mkString
}

def evaluatorOne(input: List[String]): Int = {
    val doubles = input.count(line => checkLine(line, 2))
    val triples = input.count(line => checkLine(line, 3))
    return doubles * triples
}

def evaluatorTwo(input: List[String]): String = {
    return input.combinations(2).collectFirst {
        case List(a, b) if diff(a, b) == 1 => common(a, b)
    }.get
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day02.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}