package day06

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Problem(columns: List[String], op: List[Long] => Long)

def parseInput(lines: List[String]): List[Problem] = {
    val maxLength = lines.init.map(_.length).max

    val indices = (0 until maxLength).filter(idx => {
        lines.init.forall(_(idx).isWhitespace)
    })

    val worksheet = (-1 +: indices :+ maxLength).sliding(2).collect { 
        case Seq(start, end) => lines.init.map(_.substring(start + 1, end))
    }.toList

    val operations = lines.last.split(" ").filterNot(_.isEmpty).collect {
        case "+" => (it: List[Long]) => it.sum
        case "*" => (it: List[Long]) => it.product
    }

    return (worksheet zip operations).map(Problem(_, _))
}

def helper(problems: List[Problem], parser: List[String] => List[Long]): Long = {
    problems.map { case Problem(columns, operation) => operation(parser(columns)) }.sum
}

def parserOne(problem: List[String]) = problem.map(_.trim.toLong)
def parserTwo(problem: List[String]) = problem.transpose.map(_.mkString.trim.toLong)

def evaluatorOne(input: List[Problem]) = helper(input, parserOne)
def evaluatorTwo(input: List[Problem]) = helper(input, parserTwo)

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