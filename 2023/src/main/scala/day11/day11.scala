package day11

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Position(y: Int, x: Int)

def emptyRows(map: List[List[Char]]) = map.zipWithIndex.collect {
    case (row, idx) if row.forall(_ == '.') => idx
}

def findAll(map: List[String]): List[Position] = {
    return (for {
        (line, y) <- map.zipWithIndex
        (ch, x) <- line.zipWithIndex
        if ch == '#'
    } yield Position(y, x)).toList
}

def distance(i1: Int, i2: Int, expansion: Int, isEmpty: Int => Boolean): Long = {
    val a = math.min(i1, i2)
    val d = math.abs(i1 - i2)
    return d + expansion * (a until a + d).count(isEmpty)
}

def solve(input: List[String], expansion: Int): Long = {
    val map = input.map(_.toList)

    val isRowEmpty = emptyRows(map).toSet.contains
    val isColEmpty = emptyRows(map.transpose).toSet.contains

    val galaxies = findAll(input)

    return (for {
        g <- galaxies.combinations(2)
        d1 = distance(g(0).y, g(1).y, expansion, isRowEmpty)
        d2 = distance(g(0).x, g(1).x, expansion, isColEmpty)
    } yield d1 + d2).sum
}

def evaluatorOne(input: List[String]): Long = solve(input, 1)
def evaluatorTwo(input: List[String]): Long = solve(input, 999999)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day11.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}