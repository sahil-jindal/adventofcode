package day09

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Set
import scala.util.control.Breaks._

case class Direction(dy: Int, dx: Int)

case class Knot(y: Int, x: Int) {
    def +(dir: Direction) = Knot(y + dir.dy, x + dir.dx)
    def -(that: Knot) = Direction(y - that.y, x - that.x)
}

def getDirections(dir: Char) = dir match {
    case 'U' => Direction(-1, 0)
    case 'D' => Direction(1, 0)
    case 'L' => Direction(0, -1)
    case 'R' => Direction(0, 1)
    case _ => throw new Exception()
}

def parseInput(input: List[String]) = input.flatMap(line => {
    val Array(a, b) = line.split(" ")
    List.fill(b.toInt)(getDirections(a.head)) 
})

def solver(directions: List[Direction], ropelength: Int): Int = {
    val rope = Array.fill(ropelength)(Knot(0, 0))
    val res = Set(rope.last)

    for (dir <- directions) { 
        rope(0) += dir

        breakable {
            for (i <- 1 until rope.length) {
                val Direction(dy, dx) = rope(i - 1) - rope(i)
                if (!(dy.abs == 2 && dx.abs <= 2) && !(dx.abs == 2 && dy.abs <= 2)) break()
                rope(i) += Direction(dy.sign, dx.sign)
            }
        }

        res += rope.last
    }

    return res.size
}

def evaluatorOne(directions: List[Direction]): Int = solver(directions, 2)
def evaluatorTwo(directions: List[Direction]): Int = solver(directions, 10)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day09.txt") match {
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