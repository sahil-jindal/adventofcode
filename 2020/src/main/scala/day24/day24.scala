package day24

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

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
    val tiles = Map.empty[Tile, Boolean].withDefaultValue(false)

    for (line <- input) {
        val tile = walk(line)
        tiles(tile) = !tiles(tile)
    }

    return tiles.collect { case (tile, true) => tile }.toSet
}

def neighbourhood(tile: Tile) = HexDirections.values.map(tile + _).toSet

def flip(blackTiles: Set[Tile]): Set[Tile] = {
    val tiles = blackTiles.flatMap(neighbourhood)

    return tiles.filter(tile => {
        val blacks = neighbourhood(tile).count(blackTiles.contains)
        val isBlack = blackTiles.contains(tile)
        (isBlack && (blacks == 1 || blacks == 2)) || (!isBlack && blacks == 2)
    })
}

def solve(input: List[String]) = {
    var tiles = parseBlackTiles(input)

    println(s"Part One: ${tiles.size}")

    for (_ <- 1 to 100) tiles = flip(tiles)
    
    println(s"Part Two: ${tiles.size}")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day24.txt") match {
        case Success(lines) => solve(lines)
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}