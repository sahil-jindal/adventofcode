package day04

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Point(y: Int, x: Int)

def parseInput(grid: List[String]): Set[Point] = {
    return (for {
        (line, y) <- grid.zipWithIndex
        (ch, x) <- line.zipWithIndex
        if ch == '@'
    } yield Point(y, x)).toSet
}

def getNeighbours(pt: Point) = {
    (for {
        dy <- -1 to 1
        dx <- -1 to 1
        if dy != 0 || dx != 0
    } yield Point(pt.y + dy, pt.x + dx))
}

def rollsCondition(points: Set[Point], pt: Point): Boolean = {
    return getNeighbours(pt).count(points.contains) < 4
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