package day06

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.control.Breaks._

def parseInput(lines: String): Vector[Int] = {
    return "(\\d+)".r.findAllIn(lines).map(_.toInt).toVector
}

def redistribute(banks: Vector[Int]): Vector[Int] = {
    val numBanks = banks.length
    
    val (maxValue, maxIdx) = banks.zipWithIndex.maxBy { case (v, i) => (v, -i) }

    val quotient = maxValue / numBanks
    val remainder = maxValue % numBanks

    val afterQuotient = banks.updated(maxIdx, 0).map(_ + quotient)
    val start = (maxIdx + 1) % numBanks

    val indicesToIncrement = (0 until remainder).map { j => (start + j) % numBanks }

    return indicesToIncrement.foldLeft(afterQuotient) { (current, idx) =>
        current.updated(idx, current(idx) + 1)
    }
}

def getStepCount(input: Vector[Int]) = {
    var current = input
    var seen = Map(current -> 0)
    var steps = 0
    var part1 = 0
    var part2 = 0

    breakable {
        while (true) {
            steps += 1
            current = redistribute(current)
            
            seen.get(current) match {
                case Some(previousStep) =>
                    part1 = steps
                    part2 = steps - previousStep
                    break()
                case None =>
                    seen += (current -> steps)
            }
        }
    }

    println(s"Part One: $part1")
    println(s"Part Two: $part2")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day06.txt") match {
        case Success(lines) => {
            getStepCount(parseInput(lines.head))
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}