package day03

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(line: String) = 
    raw"(\d+)".r.findAllIn(line).toArray.map(_.toInt)

def validTriangles(triangles: Array[Array[Int]]) = 
    triangles.count(triangle => {
        val Array(a, b, c) = triangle
        a + b > c && b + c > a && c + a > b
    })
    
def evaluatorOne(triangles: Array[Array[Int]]) = validTriangles(triangles)

def evaluatorTwo(triangles: Array[Array[Int]]) =
    val columnWiseTriangles = triangles.transpose.flatMap(_.grouped(3).toArray)
    validTriangles(columnWiseTriangles)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("day03.txt") match
        case Success(lines) => {
            val triangleSides = lines.map(parseInput).toArray
            println(s"Part One: ${evaluatorOne(triangleSides)}")
            println(s"Part Two: ${evaluatorTwo(triangleSides)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }