package day08

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer

case class Vec2D(y: Int, x: Int) {
    def +(that: Vec2D) = Vec2D(y + that.y, x + that.x)
    def -(that: Vec2D) = Vec2D(y - that.y, x - that.x)
}

type Grid = Map[Vec2D, Char]

def getMap(input: List[String]): Grid = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Vec2D(y, x) -> ch).toMap
}

def getUniquePositions(input: List[String], getAntiNodes: (Vec2D, Vec2D, Grid) => Seq[Vec2D]): Int = {
    val grid = getMap(input)

    val antennaLocations = grid.keys.toSeq.filter { 
        pos => grid(pos).isLetterOrDigit
    }

    return (for {
        srcAntenna <- antennaLocations
        dstAntenna <- antennaLocations
        if srcAntenna != dstAntenna && grid(srcAntenna) == grid(dstAntenna)
    } yield getAntiNodes(srcAntenna, dstAntenna, grid)).flatten.toSet.size
}

def getAntiNodesOne(srcAntenna: Vec2D, dstAntenna: Vec2D, grid: Grid): Seq[Vec2D] = {
    val dir = dstAntenna - srcAntenna
    val antiNode = dstAntenna + dir
    if (grid.contains(antiNode)) return Seq(antiNode)
    return Seq.empty
}

def getAntiNodesTwo(srcAntenna: Vec2D, dstAntenna: Vec2D, grid: Grid): Seq[Vec2D] = {
    val dir = dstAntenna - srcAntenna
    var antiNode = dstAntenna
    val res = ListBuffer.empty[Vec2D]

    while (grid.contains(antiNode)) {
        res += antiNode
        antiNode += dir
    }

    return res.toSeq
}

def evaluatorOne(input: List[String]): Int = getUniquePositions(input, getAntiNodesOne)
def evaluatorTwo(input: List[String]): Int = getUniquePositions(input, getAntiNodesTwo)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day08.txt") match {
        case Success(lines) => {        
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part One: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}