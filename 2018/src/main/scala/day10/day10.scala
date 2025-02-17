package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.annotation.tailrec

case class Point(val x: Int, val y: Int, val vx: Int, val vy: Int)

case class Plane(val minX: Int, val maxX: Int, val minY: Int, val maxY: Int) {
    def area = (maxX - minX).toLong * (maxY - minY).toLong
}

def parseInput(lines: List[String]): List[Point] = lines.map(line => {
    val nums = "[-]?\\d+".r.findAllIn(line).map(_.toInt).toVector
    Point(nums(0), nums(1), nums(2), nums(3))
})

def step(points: List[Point]): List[Point] = {
    return points.map(p => Point(p.x + p.vx, p.y + p.vy, p.vx, p.vy))
}

def getBounds(points: List[Point]): Plane = {
    val xs = points.map(_.x)
    val ys = points.map(_.y)
    return Plane(xs.min, xs.max, ys.min, ys.max)
}

def printPoints(points: List[Point]): Unit = {
    val Plane(xmin, xmax, ymin, ymax) = getBounds(points)
    val pointSet = points.map(p => (p.x, p.y)).toSet
    
    for (y <- ymin to ymax) {
        for (x <- xmin to xmax) {
            if (pointSet.contains((x, y))) {
                print("#")
            } else {
                print(".")
            }
        }
        
        println()
    }

    println()
}

@tailrec
def findMessage(points: List[Point], seconds: Int = 0): (List[Point], Int) = {
    val area = getBounds(points).area
    val nextPoints = step(points)
    val nextArea = getBounds(nextPoints).area

    if (nextArea > area) return (points, seconds) // Message found (or at least, area is increasing)
    return findMessage(nextPoints, seconds + 1)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day10.txt") match {
        case Success(lines) => {
            val (messagePoints, seconds) = findMessage(parseInput(lines))
            println(s"Seconds: $seconds")
            printPoints(messagePoints)
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}