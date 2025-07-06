package day06

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map
import scala.util.boundary, boundary.break

def parseInput(lines: String) = raw"(\d+)".r.findAllIn(lines).map(_.toInt).toVector

def redistribute(banks: Vector[Int]): Vector[Int] = {
    val numBanks = banks.length
    
    val (maxValue, maxIdx) = banks.zipWithIndex.maxBy { case (v, i) => (v, -i) }

    val quotient = maxValue / numBanks
    val remainder = maxValue % numBanks

    val afterQuotient = banks.updated(maxIdx, 0).map(_ + quotient)
    val start = (maxIdx + 1) % numBanks

    val indicesToIncrement = (0 until remainder).map { j => (start + j) % numBanks }

    return indicesToIncrement.foldLeft(afterQuotient) { case (current, idx) =>
        current.updated(idx, current(idx) + 1)
    }
}

def getStepCount(input: Vector[Int]): (Int, Int) = {
    var current = input
    val seen = Map(current -> 0)

    boundary {
        for (steps <- Iterator.from(1)) {
            current = redistribute(current)
            if (seen.contains(current)) break((steps, steps - seen(current)))
            seen(current) = steps
        }
    
        (0, 0) // Not reachable
    }    
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day06.txt") match {
        case Success(lines) => {
            val (partOne, partTwo) = getStepCount(parseInput(lines.head))
            println(s"Part One: $partOne")
            println(s"Part Two: $partTwo")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}