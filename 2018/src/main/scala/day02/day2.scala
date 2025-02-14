package day02

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def checkLine(line: String, n: Int): Boolean = {
    return line.groupBy(identity).values.map(_.length).exists(_ == n)
}

def diff(line1: String, line2: String): Int = {
    return (line1 zip line2).count { case (a, b) => a != b }
}

def common(line1: String, line2: String): String = {
    return (line1 zip line2).collect { case (a, b) if a == b => a }.mkString
}

def evaluatorOne(lines: List[String]): Int = {
    val doubles = lines.count(line => checkLine(line, 2))
    val triples = lines.count(line => checkLine(line, 3))
    return doubles * triples
}

def evaluatorTwo(lines: List[String]): String = {
    return (for {
        i <- lines.indices
        j <- i + 1 until lines.length
        line1 = lines(i)
        line2 = lines(j)
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