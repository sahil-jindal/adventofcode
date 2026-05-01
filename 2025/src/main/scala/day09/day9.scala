package day09

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

extension [A](items: List[A]) {
    def upperTriangle(): List[(A, A)] = {
        val partOne = items.tail.tails.toVector
        return (items zip partOne).init.flatMap {
            case (e1, ahead) => ahead.map(e1 -> _)
        }
    }
}

case class Point(x: Long, y: Long)

case class Inclusive(start: Long, end: Long) {
    val length: Long = end - start + 1
    def strictOverlaps(that: Inclusive) = start < that.end && that.start < end 
}

case class Rectangle(xRange: Inclusive, yRange: Inclusive) {
    def area = yRange.length * xRange.length

    def aabbCollision(that: Rectangle): Boolean = {
        xRange.strictOverlaps(that.xRange) &&
        yRange.strictOverlaps(that.yRange)
    }
}

def parseInput(input: List[String]) = input.collect {
    case s"$x,$y" => Point(x.toLong, y.toLong)
}

def formRectangle(a: Point, b: Point): Rectangle = {
    val xRange = Inclusive(a.x.min(b.x), a.x.max(b.x))
    val yRange = Inclusive(a.y.min(b.y), a.y.max(b.y))
    return Rectangle(xRange, yRange)
}

def allPossibleRectangles(points: List[Point]): List[Rectangle] = {
    return points.upperTriangle().map(formRectangle)
}

def evaluatorOne(points: List[Point]): Long = {
    return allPossibleRectangles(points).map(_.area).max
}

def evaluatorTwo(points: List[Point]): Long = {
    val rightShifted = points.tail :+ points.head
    val segments = (points zip rightShifted).map(formRectangle)

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