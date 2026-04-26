package day03

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Vec2D(y: Int, x: Int)

def treeCount(input: List[String], slopes: Vec2D*): Long = {
    val (height, width) = (input.length, input(0).length)
    
    def countTrees(dir: Vec2D): Long = { 
        return (0 until height / dir.y)
            .map(i => Vec2D(i * dir.y, (i * dir.x) % width))
            .count { case Vec2D(y, x) => input(y)(x) == '#' }.toLong
    }

    return slopes.map(countTrees).product
}

def evaluatorOne(input: List[String]): Long = treeCount(input, Vec2D(1, 3))
def evaluatorTwo(input: List[String]): Long = treeCount(input, Vec2D(1, 1), Vec2D(1, 3), Vec2D(1, 5), Vec2D(1, 7), Vec2D(2, 1))

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day03.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}