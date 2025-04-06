package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def solve(input: List[String], steps: Int): Long = {
    val List(first, second) = groupLines(input)

    val polymer = first(0)

    val generatedElement = second.map(line => {
        val parts = line.split(" -> ")
        parts(0) -> parts(1)(0)
    }).toMap

    var moleculeCount = polymer.sliding(2).toSeq.groupMapReduce(identity)(_ => 1L)(_ + _)

    for (_ <- 1 to steps) {
        val emptyMap = Map.empty[String, Long].withDefaultValue(0L)

        moleculeCount = moleculeCount.foldLeft(emptyMap) { case (updatedMap, (molecule, count)) => 
            val (a, n, b) = (molecule(0), generatedElement(molecule), molecule(1))
            updatedMap.updated(s"$a$n", updatedMap(s"$a$n") + count).updated(s"$n$b", updatedMap(s"$n$b") + count)
        }
    }

    val elementCounts = mutable.Map.empty[Char, Long].withDefaultValue(0L)

    for ((molecule, count) <- moleculeCount) {
        elementCounts(molecule(0)) += count
    }

    elementCounts(polymer.last) += 1

    return elementCounts.values.max - elementCounts.values.min
}

def evaluatorOne(input: List[String]): Long = solve(input, 10)
def evaluatorTwo(input: List[String]): Long = solve(input, 40)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day14.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}