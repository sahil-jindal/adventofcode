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

case class Plane(minX: Int, minY: Int, w: Int, h: Int) {
    def area = w.toLong * h.toLong
}

def parseInput(input: List[String]) = input.map(line => {
    val Seq(px, py, vx, vy) = raw"(-?\d+)".r.findAllIn(line).map(_.toInt).toSeq
    Point(Vec2D(py, px), Vec2D(vy, vx))
})

def getBounds(points: List[Point]): Plane = {
    val xs = points.map(_.pos.x)
    val ys = points.map(_.pos.y)
    return Plane(xs.min, ys.min, xs.max - xs.min, ys.max - ys.min)
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
    val Plane(minX, minY, w, h) = getBounds(points)
    val pointSet = points.map(_.pos).toSet
    val grid = Array.fill(h + 1, w + 1)(' ')
    
    for {
        y <- 0 to h 
        x <- 0 to w
        pos = Vec2D(minY + y, minX + x) 
        if pointSet.contains(pos)
    } grid(y)(x) = '#'
        
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