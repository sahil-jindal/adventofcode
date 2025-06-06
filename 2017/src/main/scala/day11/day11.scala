package day11

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: String) = input.split(',').toList

def wander(directions: List[String]): List[(Int, Int, Int)] = {
    return directions.scanLeft((0, 0, 0)) {
        case ((x, y, z), dir) => dir match {
            case "n"  => (x, y + 1, z - 1)
            case "s"  => (x, y - 1, z + 1)
            case "ne" => (x + 1, y, z - 1)
            case "sw" => (x - 1, y, z + 1)
            case "se" => (x + 1, y - 1, z)
            case "nw" => (x - 1, y + 1, z)
            case _    => throw new IllegalArgumentException(dir)
        }
    }
}

def distances(input: List[String]): List[Int] = {
    return wander(input).map { case (x, y, z) => (x.abs + y.abs + z.abs) / 2 }
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day11.txt") match {
        case Success(lines) => {
            val result = distances(parseInput(lines.head))
            println(s"Part One: ${result.last}")
            println(s"Part Two: ${result.max}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}