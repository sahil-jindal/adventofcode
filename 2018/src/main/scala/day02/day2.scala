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
    return (for {
        i <- input.indices
        j <- i + 1 until input.length
        line1 = input(i)
        line2 = input(j)
        if diff(line1, line2) == 1
    } yield common(line1, line2)).head
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