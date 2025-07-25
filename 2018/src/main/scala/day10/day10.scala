package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.annotation.tailrec

case class Vec2D(y: Int, x: Int) {
    def +(that: Vec2D) = Vec2D(y + that.y, x + that.x)
}

case class Point(pos: Vec2D, vel: Vec2D) {
    def move = copy(pos = pos + vel)
}

case class Plane(startY: Int, startX: Int, height: Int, width: Int) {
    def area = height.toLong * width.toLong
}

def parseInput(input: List[String]) = input.map(line => {
    val Seq(px, py, vx, vy) = raw"(-?\d+)".r.findAllIn(line).map(_.toInt).toSeq
    Point(Vec2D(py, px), Vec2D(vy, vx))
})

def getBounds(points: List[Point]): Plane = {
    val positions = points.map(_.pos)
    val (ys, xs) = (positions.map(_.y), positions.map(_.x))
    return Plane(ys.min, xs.min, ys.max - ys.min, xs.max - xs.min)
}

@tailrec
def findMessage(points: List[Point], seconds: Int = 0): (List[Point], Int) = {
    val area = getBounds(points).area
    val nextPoints = points.map(_.move)
    val nextArea = getBounds(nextPoints).area

    if (nextArea > area) return (points, seconds) // Message found (or at least, area is increasing)
    return findMessage(nextPoints, seconds + 1)
}

def makeMessage(points: List[Point]): String = {
    val Plane(startY, startX, height, width) = getBounds(points)
    val pointSet = points.map(_.pos).toSet
    val grid = Array.fill(height + 1, width + 1)(' ')
    
    pointSet.foreach(pos => grid(pos.y - startY)(pos.x - startX) = '#') 
        
    return grid.map(_.mkString).mkString("\n")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day10.txt") match {
        case Success(lines) => {
            val (messagePoints, seconds) = findMessage(parseInput(lines))
            println(s"Part Two: $seconds")
            println(s"Part One:\n${makeMessage(messagePoints)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}