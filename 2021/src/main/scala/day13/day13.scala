package day13

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Point(y: Int, x: Int)

def foldX(x: Int)(p: Point): Point = {
    if (p.x > x) p.copy(x = 2*x - p.x) else p
}

def foldY(y: Int)(p: Point): Point = {
    if (p.y > y) p.copy(y = 2*y - p.y) else p
}

def getHolds(input: List[String]): List[Set[Point]] = {
    val idx = input.indexWhere(_.trim.isEmpty)

    val points = input.take(idx).collect { 
        case s"$x,$y" => Point(y.toInt, x.toInt) 
    }.toSet

    val folds = input.drop(idx + 1).map(_.stripPrefix("fold along ")).collect { 
        case s"x=$num" => foldX(num.toInt)
        case s"y=$num" => foldY(num.toInt)
    }

    return folds.scanLeft(points) { case (pts, f) => pts.map(f) }.tail
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