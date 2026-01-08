package day09

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Point(x: Long, y: Long)

case class Rectangle(a: Point, b: Point) {
    val top = math.min(a.y, b.y)
    val bottom = math.max(a.y, b.y)
    val left = math.min(a.x, b.x)
    val right = math.max(a.x, b.x)
    
    def area = (bottom - top + 1) * (right - left + 1)

    def aabbCollision(that: Rectangle): Boolean = {
        var aIsToTheLeft = right <= that.left
        var aIsToTheRight = left >= that.right
        var aIsAbove = bottom <= that.top
        var aIsBelow = top >= that.bottom
        return !(aIsToTheRight || aIsToTheLeft || aIsAbove || aIsBelow)
    }
}

def parseInput(input: List[String]) = input.collect {
    case s"$x,$y" => Point(x.toLong, y.toLong)
}

def allPossibleRectangles(points: List[Point]): List[Rectangle] = {
    return points.combinations(2).map(it => Rectangle(it(0), it(1))).toList
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