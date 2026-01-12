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

type Pair = (Direction, Int)

def parseInput(input: List[String]) = input.collect {
    case s"U $num" => Direction(-1, 0) -> num.toInt
    case s"D $num" => Direction(1, 0) -> num.toInt
    case s"L $num" => Direction(0, -1) -> num.toInt
    case s"R $num" => Direction(0, 1) -> num.toInt
}

def solver(input: List[Pair], ropelength: Int): Int = {
    val directions = input.flatMap { case (dir, num) => List.fill(num)(dir) }
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

def evaluatorOne(input: List[Pair]): Int = solver(input, 2)
def evaluatorTwo(input: List[Pair]): Int = solver(input, 10)

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