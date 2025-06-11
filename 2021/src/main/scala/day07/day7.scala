package day07

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: String) = input.split(",").map(_.toInt).toList

def fuelMin(positions: List[Int], fuelConsumption: Int => Int): Int = {
    return (positions.min to positions.max).map(target => {
        positions.map(it => fuelConsumption((target - it).abs)).sum
    }).min
}

def evaluatorOne(positions: List[Int]): Int = fuelMin(positions, identity)
def evaluatorTwo(positions: List[Int]): Int = fuelMin(positions, dist => dist * (dist + 1) / 2)

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