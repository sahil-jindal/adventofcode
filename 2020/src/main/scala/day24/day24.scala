package day24

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

case class Tile(x: Int, y: Int)

val HexDirections = Map(
    "e"  -> (2, 0),  "w"  -> (-2, 0),
    "ne" -> (1, 1),  "nw" -> (-1, 1),
    "se" -> (1, -1), "sw" -> (-1, -1)
)

def walk(line: String): Tile = {
    var (x, y) = (0, 0)
    var remaining = line
    
    while (remaining.nonEmpty) {
        HexDirections.foreach { case (dir, (dx, dy)) =>
            if (remaining.startsWith(dir)) {
                remaining = remaining.drop(dir.length)
                x += dx
                y += dy
            }
        }
    }

    return Tile(x, y)
}

def parseBlackTiles(input: List[String]): Set[Tile] = {
    val tiles = Map.empty[Tile, Boolean].withDefaultValue(false)

    for (line <- input) {
        val tile = walk(line)
        tiles(tile) = !tiles(tile)
    }

    return tiles.collect { case (tile, true) => tile }.toSet
}

def neighbourhood(tile: Tile): Set[Tile] = HexDirections.values.map { 
    case (dx, dy) => Tile(tile.x + dx, tile.y + dy) 
}.toSet

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