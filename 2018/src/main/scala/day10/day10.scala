package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.annotation.tailrec

case class Point(val x: Int, val y: Int, val vx: Int, val vy: Int)

case class Plane(val minX: Int, val minY: Int, val w: Int, val h: Int) {
    def area = w.toLong * h.toLong
}

def parseInput(lines: List[String]): List[Point] = lines.map(line => {
    val nums = "[-]?\\d+".r.findAllIn(line).map(_.toInt).toVector
    Point(nums(0), nums(1), nums(2), nums(3))
})

def getBounds(points: List[Point]): Plane = {
    val xs = points.map(_.x)
    val ys = points.map(_.y)
    return Plane(xs.min, ys.min, xs.max - xs.min, ys.max - ys.min)
}

def step(points: List[Point]): List[Point] = {
    return points.map(p => Point(p.x + p.vx, p.y + p.vy, p.vx, p.vy))
}

@tailrec
def findMessage(points: List[Point], seconds: Int = 0): (List[Point], Int) = {
    val area = getBounds(points).area
    val nextPoints = step(points)
    val nextArea = getBounds(nextPoints).area

    if (nextArea > area) return (points, seconds) // Message found (or at least, area is increasing)
    return findMessage(nextPoints, seconds + 1)
}

def makeMessage(points: List[Point]): String = {
    val Plane(minX, minY, w, h) = getBounds(points)
    val pointSet = points.map(p => (p.x, p.y)).toSet
    val grid = Array.fill(h + 1, w + 1)(' ')
    
    for (y <- 0 to h; x <- 0 to w; if pointSet.contains((minX + x, minY + y))) grid(y)(x) = '#'
        
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