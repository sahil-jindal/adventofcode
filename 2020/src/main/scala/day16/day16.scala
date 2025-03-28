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

def parseNumbers(line: String): List[Int] = raw"(\d+)".r.findAllIn(line).map(_.toInt).toList

def parseInput(input: List[String]): Problem = {
    val blocks = groupLines(input)

    val fields = blocks.head.map(line => {
        val List(s1, e1, s2, e2) = parseNumbers(line)
        Field(line.split(":").head, n => { (n >= s1 && n <= e1) || (n >= s2 && n <= e2) })
    })

    val tickets = blocks.tail.flatMap(_.tail.map(parseNumbers))

    return Problem(fields, tickets)
}

def fieldCandidates(fields: Seq[Field], values: Int*): Seq[Field] = {
    return fields.filter(field => values.forall(field.isValid))
}

def evaluatorOne(problem: Problem): Int = {
    return problem.tickets.flatten.filterNot(value => fieldCandidates(problem.fields, value).nonEmpty).sum
}

def evaluatorTwo(problem: Problem): Long = {
    val tickets = problem.tickets.filter(_.forall(value => fieldCandidates(problem.fields, value).nonEmpty))

    val fields = Set(problem.fields*)
    val columns = Set((0 until fields.size)*)

    var res = 1L

    while columns.nonEmpty do {
        for (column <- columns) {
            val valuesInColumn = tickets.map(_(column))
            val candidates = fieldCandidates(fields.toSeq, valuesInColumn*)
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