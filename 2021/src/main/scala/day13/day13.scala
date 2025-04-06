package day13

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Point(x: Int, y: Int)

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def foldX(d: Set[Point], x: Int): Set[Point] = d.map(p => if p.x > x then p.copy(x = 2*x - p.x) else p)
def foldY(d: Set[Point], y: Int): Set[Point] = d.map(p => if p.y > y then p.copy(y = 2*y - p.y) else p)

def getHolds(input: List[String]): List[Set[Point]] = {
    val List(first, second) = groupLines(input)

    val points = first.map(line => {
        val Array(x, y) = line.split(",")
        Point(x.toInt, y.toInt)
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

    val grid = Array.ofDim[Char](height + 1, width + 1)

    for(y <- grid.indices; x <- grid(y).indices) {
        grid(y)(x) = if d.contains(Point(x, y)) then '#' else ' '
    }

    return grid.map(_.mkString("")).mkString("\n")
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