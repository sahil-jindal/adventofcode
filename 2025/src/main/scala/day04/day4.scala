package day04

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Direction(dy: Int, dx: Int) {
    def +(that: Direction) = Direction(dy + that.dy, dx + that.dx)
}

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

val N = Direction(-1, 0)
val S = Direction(1, 0)
val E = Direction(0, 1)
val W = Direction(0, -1)
val NE = N + E
val NW = N + W
val SE = S + E
val SW = S + W

val directions = List(NW, N, NE, E, SE, S, SW, W)

def parseInput(grid: List[String]): Set[Point] = {
    return (for {
        (line, y) <- grid.zipWithIndex
        (ch, x) <- line.zipWithIndex
        if ch == '@'
    } yield Point(y, x)).toSet
}

def rollsCondition(points: Set[Point], pt: Point): Boolean = {
    return directions.map(pt + _).count(points.contains) < 4
}

def next(points: Set[Point]): Set[Point] = {
    return points -- points.filter(pt => rollsCondition(points, pt))
}

def evaluatorOne(points: Set[Point]): Int = {
    return points.count(it => rollsCondition(points, it))
}

def evaluatorTwo(initial: Set[Point]): Int = {
    // detect first stable state
    val finalState = Iterator.iterate(initial)(next).sliding(2)
        .dropWhile { case Seq(a, b) => a != b }
        .next().head

    return initial.size - finalState.size
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day04.txt") match {
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