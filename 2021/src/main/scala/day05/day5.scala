package day05

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Vec2D(x: Int, y: Int) {
    def sign = Vec2D(x.sign, y.sign)
    def *(num: Int) = Vec2D(x * num, y * num)
    def +(that: Vec2D) = Vec2D(x + that.x, y + that.y)
    def -(that: Vec2D) = Vec2D(x - that.x, y - that.y)
}

case class Segment(start: Vec2D, end: Vec2D)

def parseInput(input: List[String]) = input.map(line => {
    val Seq(sx, sy, ex, ey) = raw"(\d+)".r.findAllIn(line).map(_.toInt).toSeq
    Segment(Vec2D(sx, sy), Vec2D(ex, ey))
})

def parseLines(input: List[Segment], skipDiagonals: Boolean): Seq[Seq[Vec2D]] = {
    return (for {
        Segment(start, end) <- input
        displacement = end - start
        dir = displacement.sign
        length = 1 + math.max(displacement.x.abs, displacement.y.abs)
        if !skipDiagonals || dir.x == 0 || dir.y == 0
    } yield (0 until length).map(start + dir * _))
}

def getIntersections(lines: Seq[Seq[Vec2D]]): Seq[Vec2D] = {
    return lines.flatten.groupBy(identity).collect { case (pt, occurrences) if occurrences.size > 1 => pt }.toSeq
}

def evaluatorOne(input: List[Segment]): Int = getIntersections(parseLines(input, true)).size
def evaluatorTwo(input: List[Segment]): Int = getIntersections(parseLines(input, false)).size

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