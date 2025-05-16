package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

def solve(input: List[String], steps: Int): Long = {
    val polymer = input.head

    val template = input.drop(2).map(line => {
        val parts = line.split(" -> ")
        parts(0) -> parts(1)(0)
    }).toMap

    var moleculeCount = polymer.sliding(2).toSeq.groupMapReduce(identity)(_ => 1L)(_ + _)

    for (_ <- 1 to steps) {
        val updatedMap = Map.empty[String, Long].withDefaultValue(0L)

        for ((molecule, count) <- moleculeCount) { 
            val (a, n, b) = (molecule(0), template(molecule), molecule(1))
            updatedMap(s"$a$n") += count
            updatedMap(s"$n$b") += count
        }

        moleculeCount = updatedMap.toMap
    }

    val elementCounts = Map.empty[Char, Long].withDefaultValue(0L)

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