package day07

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

extension (a: Int) {
    def midpoint(b: Int): Int = {
        return (a & b) + ((a ^ b) >> 1)
    }
}

def parseInput(input: String) = input.split(",").map(_.toInt).toVector

def evaluatorOne(positions: Vector[Int]): Int = {
    val crabs = positions.sorted
    val half = positions.size >> 1
    
    val median = 
        if ((crabs.size & 1) == 1) then crabs(half)
        else crabs(half - 1).midpoint(crabs(half))

    return crabs.map(n => (n - median).abs).sum
}

def evaluatorTwo(positions: Vector[Int]): Int = {
    val mean = positions.sum / positions.size

    def triangle(x: Int, mean: Int): Int = {
        val n = (x - mean).abs
        return (n * (n + 1)) / 2
    }

    return (-1 to 1).map(delta => {
        positions.map(x => triangle(x, mean + delta)).sum
    }).min
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day07.txt") match {
        case Success(lines) => {
            val input = parseInput(lines.head)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}