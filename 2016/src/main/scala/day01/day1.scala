package day01

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{ListBuffer, Set}

case class Point(val y: Int, val x: Int)
class Direction(val dy: Int, val dx: Int)

def parseInput(line: String) = line.split(", ").map {
    it => (it.charAt(0), it.substring(1).toInt)
}

def travel(directions: Array[(Char, Int)]): List[Point] = 
    var currPoint = Point(0, 0)
    var currDirection = Direction(-1, 0)

    val points = ListBuffer(currPoint)

    for (dir, amount) <- directions do {
        
        currDirection = dir match {
            case 'R' => Direction(currDirection.dx, -currDirection.dy)
            case 'L' => Direction(-currDirection.dx, currDirection.dy)
            case _ => Direction(0, 0)
        }

        for i <- 0 until amount do {
            currPoint = Point(currPoint.y + currDirection.dy, currPoint.x + currDirection.dx)
            points += currPoint
        }
    }

    return points.toList

def evaluatorOne(directions: Array[(Char, Int)]) =
    val dest = travel(directions).last
    dest.x.abs + dest.y.abs

def evaluatorTwo(directions: Array[(Char, Int)]) =
    val uniquePoints = Set[Point]()
    val repeatedPoint = travel(directions).find(point => !uniquePoints.add(point)).get
    repeatedPoint.x.abs + repeatedPoint.y.abs

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

@main
def hello(): Unit =
    readLinesFromFile("day01.txt") match
        case Success(lines) => {
            val directions = parseInput(lines(0))
            println(s"Part One: ${evaluatorOne(directions)}")
            println(s"Part Two: ${evaluatorTwo(directions)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }