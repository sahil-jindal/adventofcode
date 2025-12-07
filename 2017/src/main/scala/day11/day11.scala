package day11

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Vec3D(x: Int, y: Int, z: Int) {
    def len = x.abs + y.abs + z.abs
    def +(that: Vec3D) = Vec3D(x + that.x, y + that.y, z + that.z)
}

def parseInput(input: String) = input.split(',').collect {
    case "n"  => Vec3D(0, 1, -1)
    case "s"  => Vec3D(0, -1, 1)
    case "ne" => Vec3D(1, 0, -1)
    case "sw" => Vec3D(-1, 0, 1)
    case "se" => Vec3D(1, -1, 0)
    case "nw" => Vec3D(-1, 1, 0)    
}

def distances(distances: Array[Vec3D]): Array[Int] = {
    return distances.scanLeft(Vec3D(0, 0, 0))(_ + _).map(_.len / 2)
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