package day01

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Set

enum Moves { case Left, Right }

case class Pair(turn: Moves, steps: Int)
case class Direction(dy: Int, dx: Int)

case class Point(y: Int, x: Int) {
    def abs = x.abs + y.abs
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

def parseInput(input: String) = input.split(", ").collect {
    case s"L$num" => Pair(Moves.Left, num.toInt)
    case s"R$num" => Pair(Moves.Right, num.toInt)
}.toList

def updateDirection(dir: Direction, turn: Moves) = turn match {
    case Moves.Right => Direction(dir.dx, -dir.dy)
    case Moves.Left => Direction(-dir.dx, dir.dy)
}

def travel(directions: List[Pair]): List[Point] = {
    var currDirection = Direction(-1, 0)
    val allDirections = directions.scanLeft(currDirection) { case (dir, it) => updateDirection(dir, it.turn) }.tail
    val expandedDirections = (allDirections zip directions).flatMap { case (dir, it) => List.fill(it.steps)(dir) }
    return expandedDirections.scanLeft(Point(0, 0))(_ + _)
}

def evaluatorOne(path: List[Point]): Int = path.last.abs

def evaluatorTwo(path: List[Point]): Int = {
    val uniquePoints = Set.empty[Point]
    return path.find(point => !uniquePoints.add(point)).get.abs
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day01.txt") match {
        case Success(lines) => {
            val path = travel(parseInput(lines.head))
            println(s"Part One: ${evaluatorOne(path)}")
            println(s"Part Two: ${evaluatorTwo(path)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}