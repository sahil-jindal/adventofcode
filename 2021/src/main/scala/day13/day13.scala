package day13

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Point(y: Int, x: Int)

def foldX(pts: Set[Point], x: Int) = pts.map(p => if p.x > x then p.copy(x = 2*x - p.x) else p)
def foldY(pts: Set[Point], y: Int) = pts.map(p => if p.y > y then p.copy(y = 2*y - p.y) else p)

def getHolds(input: List[String]): List[Set[Point]] = {
    val idx = input.indexWhere(_.trim.isEmpty)
    val first = input.take(idx)
    val second = input.drop(idx + 1)

    val points = first.map(line => {
        val Array(x, y) = line.split(",").map(_.toInt)
        Point(y, x)
    }).toSet

    val res = second.scanLeft(points) { case (points, line) => 
        val Array(axis, number) = line.stripPrefix("fold along ").split("=")

        if (axis == "x") {
            foldX(points, number.toInt)
        } else {
            foldY(points, number.toInt)
        }
    }

    return res.tail
}

def makeMessage(points: Set[Point]): String = {
    val (height, width) = (points.map(_.y).max, points.map(_.x).max)
    val grid = Array.fill(height + 1, width + 1)(' ')

    points.foreach { case Point(y, x) => grid(y)(x) = '#' } 
    
    return grid.map(_.mkString).mkString("\n")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day13.txt") match {
        case Success(lines) => {
            val res = getHolds(lines)
            println(s"Part One: ${res.head.size}")
            println(s"Part Two:\n${makeMessage(res.last)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}