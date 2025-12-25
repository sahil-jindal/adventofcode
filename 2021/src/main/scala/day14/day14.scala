package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map => MutableMap}

type Pair = (polymer: String, template: Map[(Char, Char), Char])

def parseInput(input: List[String]): Pair = {
    val template = input.drop(2).map(line => {
        val Seq(a, b, c) = raw"(\w)".r.findAllIn(line).map(_.head).toSeq
        (a, b) -> c
    }).toMap

    return (input.head, template)
}

def solve(input: Pair, steps: Int): Long = {
    val (polymer, template) = input

    var moleculeCount = (polymer.init zip polymer.tail).groupMapReduce(identity)(_ => 1L)(_ + _)

    for (_ <- 1 to steps) {
        val updatedMap = MutableMap.empty[(Char, Char), Long].withDefaultValue(0L)

        for (((a, b), count) <- moleculeCount) { 
            val n = template((a, b))
            updatedMap((a, n)) += count
            updatedMap((n, b)) += count
        }

        moleculeCount = updatedMap.toMap
    }

    val elementCounts = MutableMap.empty[Char, Long].withDefaultValue(0L)

    for ((molecule, count) <- moleculeCount) {
        elementCounts(molecule(0)) += count
    }

    elementCounts(polymer.last) += 1

    return elementCounts.values.max - elementCounts.values.min
}

def evaluatorOne(input: Pair): Long = solve(input, 10)
def evaluatorTwo(input: Pair): Long = solve(input, 40)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day14.txt") match {
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