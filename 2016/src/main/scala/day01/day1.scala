package day01

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Set

case class Pair(turn: Char, steps: Int)
case class Direction(dy: Int, dx: Int)

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

def parseInput(input: String) = input.split(", ").map(it => Pair(it.head, it.tail.toInt)).toList

def updateDirection(dir: Direction, turn: Char) = turn match {
    case 'R' => Direction(dir.dx, -dir.dy)
    case 'L' => Direction(-dir.dx, dir.dy)
    case _   => dir // No change for invalid input
}

def travel(directions: List[Pair]): List[Point] = {
    var currDirection = Direction(-1, 0)
    val allDirections = directions.scanLeft(currDirection) { case (dir, it) => updateDirection(dir, it.turn) }.tail
    val expandedDirections = (allDirections zip directions).flatMap { case (dir, it) => List.fill(it.steps)(dir) }
    return expandedDirections.scanLeft(Point(0, 0))(_ + _)
}

def evaluatorOne(directions: List[Pair]): Int = {
    val dest = travel(directions).last
    return dest.x.abs + dest.y.abs
}

def evaluatorTwo(directions: List[Pair]): Int = {
    val uniquePoints = Set.empty[Point]
    val repeatedPoint = travel(directions).find(point => !uniquePoints.add(point)).get
    return repeatedPoint.x.abs + repeatedPoint.y.abs
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day01.txt") match {
        case Success(lines) => {
            val directions = parseInput(lines.head)
            println(s"Part One: ${evaluatorOne(directions)}")
            println(s"Part Two: ${evaluatorTwo(directions)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}