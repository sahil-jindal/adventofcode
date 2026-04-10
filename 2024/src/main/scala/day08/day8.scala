package day08

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Vec2D(y: Int, x: Int) {
    def +(that: Vec2D) = Vec2D(y + that.y, x + that.x)
    def -(that: Vec2D) = Vec2D(y - that.y, x - that.x)
}

type Grid = Map[Vec2D, Char]

def parseInput(input: List[String]): Grid = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Vec2D(y, x) -> ch).toMap
}

def getUniquePositions(grid: Grid, getAntiNodes: (Vec2D, Vec2D, Grid) => List[Vec2D]): Int = {
    val groupedAntennas = grid.filter(_._2.isLetterOrDigit).groupMap(_._2)(_._1).values.map(_.toSet)

    return (for {
        antennas <- groupedAntennas
        src <- antennas
        dst <- antennas - src
    } yield getAntiNodes(dst, dst - src, grid)).flatten.toSet.size
}

def getAntiNodesOne(dst: Vec2D, dir: Vec2D, grid: Grid): List[Vec2D] = {
    val antiNode = dst + dir
    if (grid.contains(antiNode)) return List(antiNode)
    return List.empty
}

def getAntiNodesTwo(dst: Vec2D, dir: Vec2D, grid: Grid): List[Vec2D] = {
    return Iterator.iterate(dst)(_ + dir).takeWhile(grid.contains).toList
}

def evaluatorOne(input: Grid): Int = getUniquePositions(input, getAntiNodesOne)
def evaluatorTwo(input: Grid): Int = getUniquePositions(input, getAntiNodesTwo)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day08.txt") match {
        case Success(lines) => {
            val input = parseInput(lines)        
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part One: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}