package day11

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Position(y: Int, x: Int)
case class Triplet(galaxies: List[Position], isRowEmpty: Int => Boolean, isColEmpty: Int => Boolean)

def emptyRows(grid: List[String]) = grid.zipWithIndex.collect {
    case (row, idx) if row.forall(_ == '.') => idx
}

def findAll(map: List[String]): List[Position] = {
    return (for {
        (line, y) <- map.zipWithIndex
        (ch, x) <- line.zipWithIndex
        if ch == '#'
    } yield Position(y, x)).toList
}

def parseInput(input: List[String]): Triplet = {
    val isRowEmpty = emptyRows(input).toSet.contains
    val isColEmpty = emptyRows(input.transpose.map(_.mkString)).toSet.contains

    return Triplet(findAll(input), isRowEmpty, isColEmpty)
}

def distance(i1: Int, i2: Int, expansion: Int, isEmpty: Int => Boolean): Long = {
    val d = (i1 - i2).abs
    val (start, end) = (i1.min(i2), i1.max(i2))
    return d + expansion * (start until end).count(isEmpty)
}

def solve(input: Triplet, expansion: Int): Long = {
    val Triplet(galaxies, isRowEmpty, isColEmpty) = input

    return (for {
        List(gA, gB) <- galaxies.combinations(2)
        d1 = distance(gA.y, gB.y, expansion, isRowEmpty)
        d2 = distance(gA.x, gB.x, expansion, isColEmpty)
    } yield d1 + d2).sum
}

def evaluatorOne(input: Triplet): Long = solve(input, 1)
def evaluatorTwo(input: Triplet): Long = solve(input, 999999)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day11.txt") match {
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