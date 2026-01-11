package day13

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Point(y: Int, x: Int)

sealed trait Line { def fold(p: Point): Point }

case class Vertical(x: Int) extends Line {
    override def fold(p: Point) = {
        if (p.x > x) p.copy(x = 2*x - p.x) else p
    }
}

case class Horizontal(y: Int) extends Line {
    override def fold(p: Point) = {
        if (p.y > y) p.copy(y = 2*y - p.y) else p
    }
}

type Input = (points: Set[Point], lines: List[Line])

def parseInput(input: List[String]): Input = {
    val idx = input.indexWhere(_.trim.isEmpty)

    val points = input.take(idx).collect { 
        case s"$x,$y" => Point(y.toInt, x.toInt) 
    }.toSet

    val lines = input.drop(idx + 1).map(_.stripPrefix("fold along ")).collect { 
        case s"x=$num" => Vertical(num.toInt)
        case s"y=$num" => Horizontal(num.toInt)
    }

    return (points, lines)
}

def evaluatorOne(input: Input): Set[Point] = {
    val (points, lines) = input
    return points.map(lines.head.fold)
}

def evaluatorTwo(input: Input): String = {
    var (points, lines) = input

    points = lines.foldLeft(points) { case (pts, line) => pts.map(line.fold) }

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
            val input = parseInput(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two:\n${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}