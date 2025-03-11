package day01

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Set

case class Point(val y: Int, val x: Int)
class Direction(val dy: Int, val dx: Int)

def parseInput(line: String): Array[(Char, Int)] = line.split(", ").map(it => (it.head, it.tail.toInt))

def updateDirection(currDirection: Direction, turn: Char): Direction = turn match {
    case 'R' => Direction(currDirection.dx, -currDirection.dy)
    case 'L' => Direction(-currDirection.dx, currDirection.dy)
    case _   => currDirection // No change for invalid input
}

def travel(directions: Array[(Char, Int)]): List[Point] = {
    var currDirection = Direction(-1, 0)
    val allDirections = directions.scanLeft(currDirection) { case (currDir, (turn, _)) => updateDirection(currDir, turn) }.tail.toList
    val expandedDirections = allDirections.zip(directions).flatMap { case (dir, (_, steps)) => List.fill(steps)(dir) }
    return expandedDirections.scanLeft(Point(0, 0)) { (prev, dir) => Point(prev.y + dir.dy, prev.x + dir.dx) }
}

def evaluatorOne(directions: Array[(Char, Int)]): Int = {
    val dest = travel(directions).last
    return dest.x.abs + dest.y.abs
}

def evaluatorTwo(directions: Array[(Char, Int)]): Int = {
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