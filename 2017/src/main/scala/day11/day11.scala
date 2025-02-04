package day11

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

private def wander(directions: Array[String]): Array[(Int, Int, Int)] = {
    directions.scanLeft((0, 0, 0)) {
        case ((x, y, z), dir) => dir match {
            case "n"  => (x + 0, y + 1, z - 1)
            case "ne" => (x + 1, y + 0, z - 1)
            case "se" => (x + 1, y - 1, z + 0)
            case "s"  => (x + 0, y - 1, z + 1)
            case "sw" => (x - 1, y + 0, z + 1)
            case "nw" => (x - 1, y + 1, z + 0)
            case _     => throw new IllegalArgumentException(dir)
        }
    }
}

private def distances(input: Array[String]): Array[Int] = {
    wander(input).map { case (x, y, z) => (x.abs + y.abs + z.abs) / 2 }
}

def evaluatorOne(input: Array[String]): Int = distances(input).last
def evaluatorTwo(input: Array[String]): Int = distances(input).max

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("day11.txt") match
        case Success(lines) => {
            val directions = lines(0).split(',')
            println(s"Part One: ${evaluatorOne(directions)}")
            println(s"Part Two: ${evaluatorTwo(directions)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }