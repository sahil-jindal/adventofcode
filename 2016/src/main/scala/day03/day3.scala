package day03

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(lines: List[String]) = lines.map(line => {
    raw"(\d+)".r.findAllIn(line).map(_.toInt).toList
})

def isValidTriangle(triangle: List[Int]): Boolean = {
    val List(a, b, c) = triangle
    a + b > c && b + c > a && c + a > b
}
    
def evaluatorOne(triangles: List[List[Int]]): Int = triangles.count(isValidTriangle)
def evaluatorTwo(triangles: List[List[Int]]): Int = triangles.transpose.flatten.grouped(3).count(isValidTriangle)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day03.txt") match {
        case Success(lines) => {
            val triangleSides = parseInput(lines)
            println(s"Part One: ${evaluatorOne(triangleSides)}")
            println(s"Part Two: ${evaluatorTwo(triangleSides)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}