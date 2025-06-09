package day24

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Direction(dy: Int, dx: Int)

case class Tile(y: Int, x: Int) {
    def +(dir: Direction) = Tile(y + dir.dy, x + dir.dx)
}

val HexDirections = Map(
    "e"  -> Direction(0, 2),  "w"  -> Direction(0, -2),
    "ne" -> Direction(1, 1),  "nw" -> Direction(1, -1),
    "se" -> Direction(-1, 1), "sw" -> Direction(-1, -1)
)

def walk(line: String): Tile = {
    var pos = Tile(0, 0)
    var remaining = line
    
    while (remaining.nonEmpty) {
        for ((ch, dir) <- HexDirections) {
            if (remaining.startsWith(ch)) {
                remaining = remaining.drop(ch.length)
                pos += dir
            }
        }
    }

    return pos
}

def parseBlackTiles(input: List[String]): Set[Tile] = {
    val tiles = input.map(walk).groupMapReduce(identity)(_ => 1)(_ + _)
    return tiles.collect { case (tile, n) if n % 2 == 1 => tile }.toSet
}

def neighbourhood(tile: Tile) = HexDirections.values.map(tile + _).toSet + tile

def flip(blackTiles: Set[Tile]): Set[Tile] = {
    return blackTiles.flatMap(neighbourhood).filter(tile => {
        val blacks = neighbourhood(tile).count(blackTiles.contains)
        blacks == 2 || (blacks == 3 && blackTiles.contains(tile))
    })
}

def evaluatorOne(input: Set[Tile]): Int = input.size
    
def evaluatorTwo(input: Set[Tile]): Int = {
    var tiles = input
    
    for (_ <- 1 to 100) { tiles = flip(tiles) }
    
    return tiles.size
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day24.txt") match {
        case Success(lines) => {
            var input = parseBlackTiles(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part One: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}