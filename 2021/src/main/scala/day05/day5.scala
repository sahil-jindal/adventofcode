package day05

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Vec2(x: Int, y: Int)

def parseLines(input: List[String], skipDiagonals: Boolean): Seq[Seq[Vec2]] = {
    for {
        line <- input
        ns = raw"\d+".r.findAllIn(line).map(_.toInt).toArray
        start = Vec2(ns(0), ns(1))
        end = Vec2(ns(2), ns(3))
        displacement = Vec2(end.x - start.x, end.y - start.y)
        length = 1 + math.max(displacement.x.abs, displacement.y.abs)
        dir = Vec2(displacement.x.sign, displacement.y.sign)
        if !skipDiagonals || dir.x == 0 || dir.y == 0
    } yield (0 until length).map(i => Vec2(start.x + i * dir.x, start.y + i * dir.y))
}

def getIntersections(lines: Seq[Seq[Vec2]]): Seq[Vec2] = {
    return lines.flatten.groupBy(identity).collect { case (pt, occurrences) if occurrences.size > 1 => pt }.toSeq
}

def evaluatorOne(input: List[String]): Int = getIntersections(parseLines(input, true)).size
def evaluatorTwo(input: List[String]): Int = getIntersections(parseLines(input, false)).size

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day05.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}