package day24

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Direction(dx: Int, dy: Int, dz: Int)

case class Tile(x: Int, y: Int, z: Int) {
  def +(dir: Direction) = Tile(x + dir.dx, y + dir.dy, z + dir.dz)
}

val HexDirections = Map(
    "e"  -> Direction(1, -1, 0), "w"  -> Direction(-1, 1, 0),
    "ne" -> Direction(1, 0, -1), "nw" -> Direction(0, 1, -1),
    "se" -> Direction(0, -1, 1), "sw" -> Direction(-1, 0, 1)
)

def walk(line: String): Tile = {
    var pos = Tile(0, 0, 0)
    var remaining = line
    
    while (remaining.nonEmpty) {
        val (ch, dir) = HexDirections.view.filterKeys(remaining.startsWith).head
        remaining = remaining.drop(ch.length)
        pos += dir
    }

    return pos
}

def parseBlackTiles(input: List[String]): Set[Tile] = {
    val tiles = input.groupMapReduce(walk)(_ => true)(_ ^ _)
    return tiles.collect { case (tile, true) => tile }.toSet
}

def neighbourhood(tile: Tile) = HexDirections.values.map(tile + _).toSet

def flip(blackTiles: Set[Tile]): Set[Tile] = {
    val neighborCounts = blackTiles.toSeq
        .flatMap(neighbourhood)
        .groupMapReduce(identity)(_ => 1)(_ + _)

    return neighborCounts.collect {
        case (tile, 2) => tile
        case (tile, 1) if blackTiles(tile) => tile
    }.toSet
}

def evaluatorOne(input: Set[Tile]): Int = input.size
def evaluatorTwo(input: Set[Tile]): Int = Iterator.iterate(input)(flip).drop(100).next().size

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day24.txt") match {
        case Success(lines) => {
            val input = parseBlackTiles(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part One: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}