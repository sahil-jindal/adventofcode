package day05

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Vec2D(x: Int, y: Int) {
    def *(num: Int) = Vec2D(x * num, y * num)
    def +(that: Vec2D) = Vec2D(x + that.x, y + that.y)
    def -(that: Vec2D) = Vec2D(x - that.x, y - that.y)
}

type Segment = (from: Vec2D, to: Vec2D)

def gcd(a: Int, b: Int): Int = if b == 0 then a else gcd(b, a % b)

def parseInput(input: List[String]) = input.map(line => {
    val Seq(sx, sy, ex, ey) = raw"(\d+)".r.findAllIn(line).map(_.toInt).toSeq
    (Vec2D(sx, sy), Vec2D(ex, ey))
})

def getDirection(dir: Vec2D): (Vec2D, Int) = {
    val Vec2D(y, x) = dir
    val g = gcd(y.abs, x.abs)
    return (Vec2D(y / g, x / g), g)
}

def getPoints(input: List[Segment], skipDiagonals: Boolean): List[Vec2D] = {
    return (for {
        (from, to) <- input
        (dir, steps) = getDirection(to - from)
        if !skipDiagonals || dir.x == 0 || dir.y == 0
    } yield List.tabulate(steps + 1)(from + dir * _)).flatten
}

def countCommon(points: List[Vec2D]): Int = {
    return points.groupMapReduce(identity)(_ => 1)(_ + _).values.count(_ > 1)
}

def evaluatorOne(input: List[Segment]): Int = countCommon(getPoints(input, true))
def evaluatorTwo(input: List[Segment]): Int = countCommon(getPoints(input, false))

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day05.txt") match {
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