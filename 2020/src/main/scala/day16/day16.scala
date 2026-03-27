package day16

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer

case class Range(start: Int, end: Int) {
    def contains(n: Int) = start <= n && n <= end
}

case class Rule(fieldName: String, r1: Range, r2: Range) {
    def check(n: Int) = r1.contains(n) || r2.contains(n)
}

case class Problem(rules: List[Rule], yourTicket: Vector[Int], ticketsByColumnByColumn: List[List[Int]])

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def parseNumbers(line: String) = raw"(\d+)".r.findAllIn(line).map(_.toInt).toVector

def parseInput(input: List[String]): Problem = {
    val List(first, second, third) = groupLines(input)

    val rules = first.map(line => {
        val Array(fieldName, ranges) = line.split(":", 2)
        val Vector(s1, e1, s2, e2) = parseNumbers(ranges)
        Rule(fieldName, Range(s1, e1), Range(s2, e2))
    })

    val yourTicket = parseNumbers(second.last)
    val ticketsByColumn = third.tail.map(parseNumbers).transpose

    return Problem(rules, yourTicket, ticketsByColumn)
}

def solvePartOne(rules: List[Rule], ticketsByColumn: List[List[Int]]) = {
    val sortedRanges = rules.flatMap(r => List(r.r1, r.r2)).sortBy(_.start)

    val (current, remaining) = (sortedRanges.head, sortedRanges.tail)
    
    val mergedRanges = remaining.foldLeft(List(current)) { (merged, current) =>
        val Range(lastStart, lastEnd) = merged.last
        val Range(currStart, currEnd) = current

        if (currStart <= lastEnd + 1) {
            merged.init :+ Range(lastStart, lastEnd max currEnd)
        } else {
            merged :+ current
        }
    }

    val valid = Array.fill(ticketsByColumn(0).size)(true)
    var total = 0

    for (column <- ticketsByColumn; (n, i) <- column.zipWithIndex) {
        if (!mergedRanges.exists(_.contains(n))) {
            total += n
            valid(i) = false
        }
    }

    (total, valid.toList)
}

def solvePartTwo(problem: Problem, valid: List[Boolean]): Long = {
    val Problem(rules, yourTicket, ticketsByColumn) = problem

    val rulesByColumn = ticketsByColumn.map(column => {
        val validValues = (valid zip column).collect { case (true, n) => n }
        ListBuffer.from(rules.filter(rule => validValues.forall(rule.check)))
    })

    var product = 1L

    while (rulesByColumn.exists(_.size > 1)) {
        for ((column, i) <- rulesByColumn.zipWithIndex) {
            if (column.size == 1) {
                val found = column.remove(0)
                
                if (found.fieldName.startsWith("departure")) {
                    product *= yourTicket(i)
                }

                for (remaining <- rulesByColumn) {
                    remaining.filterInPlace(_ != found)
                }
            }
        } 
    }

    return product
}

def solver(problem: Problem): (Int, Long) = {
    val (total, valid) = solvePartOne(problem.rules, problem.ticketsByColumnByColumn)
    return (total, solvePartTwo(problem, valid))
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day16.txt") match {
        case Success(lines) => {
            val (partOne, partTwo) = solver(parseInput(lines))
            println(s"Part One: $partOne")
            println(s"Part Two: $partTwo")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}