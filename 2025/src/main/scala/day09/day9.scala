package day09

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Point(x: Long, y: Long)

case class Inclusive(start: Long, end: Long) {
    val length: Long = end - start + 1
    def strictOverlaps(that: Inclusive) = start < that.end && that.start < end 
}

case class Rectangle(a: Point, b: Point) {
    val xRange = Inclusive(a.x.min(b.x), a.x.max(b.x))
    val yRange = Inclusive(a.y.min(b.y), a.y.max(b.y))
    
    def area = yRange.length * xRange.length

    def aabbCollision(that: Rectangle): Boolean = {
        xRange.strictOverlaps(that.xRange) &&
        yRange.strictOverlaps(that.yRange)
    }
}

def parseInput(input: List[String]) = input.collect {
    case s"$x,$y" => Point(x.toLong, y.toLong)
}

def allPossibleRectangles(points: List[Point]): List[Rectangle] = {
    return points.combinations(2).collect { case List(a, b) => Rectangle(a, b) }.toList
}

def boundary(points: List[Point]): List[Rectangle] = {
    val rightShifted = points.tail :+ points.head
    return (points zip rightShifted).map(Rectangle(_, _))
}

def evaluatorOne(points: List[Point]): Long = {
    return allPossibleRectangles(points).map(_.area).max
}

def evaluatorTwo(points: List[Point]): Long = {
    val segments = boundary(points)

    return allPossibleRectangles(points)
        .sortBy(_.area)(using Ordering.Long.reverse)
        .find(it => !segments.exists(it.aabbCollision)).get.area
} 

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day09.txt") match {
        case Success(lines) => {
            val input = parseInput(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}