package day03

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Vec2D(y: Int, x: Int) {
    def +(that: Vec2D) = Vec2D(y + that.y, x + that.x)
}

def treeCount(input: List[String], slopes: Vec2D*): Long = {
    val (height, width) = (input.length, input(0).length)
    
    def countTrees(dir: Vec2D): Long = { 
        return Iterator.iterate(Vec2D(0, 0))(_ + dir)
            .takeWhile(_.y < height).drop(1)
            .count { case Vec2D(y, x) => input(y)(x % width) == '#' }.toLong
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