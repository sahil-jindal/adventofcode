package day11

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(line: String): List[String] = line.split(',').toList

def wander(directions: List[String]): List[(Int, Int, Int)] = {
    directions.scanLeft((0, 0, 0)) {
        case ((x, y, z), dir) => dir match {
            case "n"  => (x, y + 1, z - 1)
            case "s"  => (x, y - 1, z + 1)
            case "ne" => (x + 1, y, z - 1)
            case "sw" => (x - 1, y, z + 1)
            case "se" => (x + 1, y - 1, z)
            case "nw" => (x - 1, y + 1, z)
            case _     => throw new IllegalArgumentException(dir)
        }
    }
}

def distances(input: List[String]): List[Int] = {
    return wander(input).map { case (x, y, z) => (x.abs + y.abs + z.abs) / 2 }
}

def evaluatorOne(input: List[String]): Int = distances(input).last
def evaluatorTwo(input: List[String]): Int = distances(input).max

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day11.txt") match {
        case Success(lines) => {
            val directions = parseInput(lines.head)
            println(s"Part One: ${evaluatorOne(directions)}")
            println(s"Part Two: ${evaluatorTwo(directions)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}