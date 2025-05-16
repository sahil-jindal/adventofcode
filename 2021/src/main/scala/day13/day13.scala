package day13

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Point(y: Int, x: Int)

def foldX(d: Set[Point], x: Int) = d.map(p => if p.x > x then p.copy(x = 2*x - p.x) else p)
def foldY(d: Set[Point], y: Int) = d.map(p => if p.y > y then p.copy(y = 2*y - p.y) else p)

def getHolds(input: List[String]): List[Set[Point]] = {
    val idx = input.indexWhere(_.trim.isEmpty)
    val first = input.take(idx)
    val second = input.drop(idx + 1)

    val points = first.map(line => {
        val Array(x, y) = line.split(",")
        Point(y.toInt, x.toInt)
    }).toSet

    val res = second.scanLeft(points) { case (points, line) => 
        val Array(first, second) = line.split("=")

        if (first.endsWith("x")) {
            foldX(points, second.toInt)
        } else {
            foldY(points, second.toInt)
        }
    }

    return res.tail
}

def toString(d: Set[Point]): String = {
    val height = d.map(_.y).max
    val width = d.map(_.x).max

    val grid = Array.fill(height + 1, width + 1)(' ')

    for (y <- grid.indices; x <- grid(y).indices) {
        if d.contains(Point(y, x)) then grid(y)(x) = '#'
    } 
    
    return grid.map(_.mkString).mkString("\n")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day13.txt") match {
        case Success(lines) => {
            val res = getHolds(lines)
            println(s"Part One: ${res.head.size}")
            println(s"Part Two:\n${toString(res.last)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}