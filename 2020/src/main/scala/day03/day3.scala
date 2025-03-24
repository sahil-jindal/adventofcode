package day03

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def treeCount(input: List[String], slopes: (Int, Int)*): Long = {
    val (height, width) = (input.length, input.head.length)
    
    return slopes.map { case (dy, dx) =>
        var (y, x) = (dy, dx)
        var trees = 0
        
        while (y < height) {
            if (input(y)(x % width) == '#') trees += 1
            y += dy
            x += dx
        }
        
        trees.toLong
    }.product
}

def evaluatorOne(input: List[String]): Long = treeCount(input, (1, 3))
def evaluatorTwo(input: List[String]): Long = treeCount(input, (1, 1), (1, 3), (1, 5), (1, 7), (2, 1))

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