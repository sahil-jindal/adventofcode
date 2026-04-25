package day06

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

type Problem = (columns: List[String], op: List[Long] => Long)

def parseInput(lines: List[String]): List[Problem] = {
    val (numberLines, operators) = (lines.init, lines.last)

    val maxLength = numberLines.map(_.length).max
    val numberStrings = numberLines.map(_.padTo(maxLength, ' '))

    val indices = numberStrings.transpose.zipWithIndex.collect {
        case (line, id) if line.forall(_.isWhitespace) => id
    }

    val allIndices = (-1 +: indices :+ maxLength)

    val worksheet = (allIndices.init zip allIndices.tail).map { 
        case (start, end) => numberStrings.map(_.substring(start + 1, end))
    }

    val operations = operators.filterNot(_.isWhitespace).collect {
        case '+' => (it: List[Long]) => it.sum
        case '*' => (it: List[Long]) => it.product
    }

    return (worksheet zip operations)
}

def helper(problems: List[Problem], parser: List[String] => List[Long]): Long = {
    problems.map { case (columns, operation) => operation(parser(columns)) }.sum
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