package day16

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Set

case class Field(name: String, isValid: Int => Boolean)
case class Problem(fields: List[Field], tickets: List[List[Int]])

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def parseNumbers(line: String) = raw"(\d+)".r.findAllIn(line).map(_.toInt).toList

def parseInput(input: List[String]): Problem = {
    val blocks = groupLines(input)

    val fields = blocks.head.map(line => {
        val Array(fieldName, ranges) = line.split(":", 2)
        val List(s1, e1, s2, e2) = parseNumbers(ranges)
        Field(fieldName, n => { (n >= s1 && n <= e1) || (n >= s2 && n <= e2) })
    })

    val tickets = blocks.tail.flatMap(_.tail.map(parseNumbers))

    return Problem(fields, tickets)
}

def fieldCandidates(fields: List[Field], values: Int*): List[Field] = {
    return fields.filter(field => values.forall(field.isValid))
}

def evaluatorOne(problem: Problem): Int = {
    return problem.tickets.flatten.filter(fieldCandidates(problem.fields, _).isEmpty).sum
}

def evaluatorTwo(problem: Problem): Long = {
    val tickets = problem.tickets.filter(_.forall(fieldCandidates(problem.fields, _).nonEmpty))

    val fields = Set.from(problem.fields)
    val columns = Set.from((0 until fields.size))

    var res = 1L

    while (columns.nonEmpty) {
        for (column <- columns) {
            val valuesInColumn = tickets.map(_(column))
            val candidates = fieldCandidates(fields.toList, valuesInColumn*)
            if (candidates.length == 1) {
                val field = candidates.head
                fields -= field
                columns -= column
                if (field.name.startsWith("departure")) {
                    res *= valuesInColumn.head
                }
            }
        }
    }

    return res
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day16.txt") match {
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