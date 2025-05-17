package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Set
import scala.util.control.Breaks._

case class Direction(dy: Int, dx: Int) {
    def unary_- = Direction(-dy, -dx)
}

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

type Grid = Map[Point, Char]

val Up = Direction(-1, 0)
val Down = Direction(1, 0)
val Left = Direction(0, -1)
val Right = Direction(0, 1)
val Dirs = List(Up, Right, Down, Left)

val Exits = Map(
    '7' -> Set(Left, Down),
    'F' -> Set(Right, Down),
    'L' -> Set(Up, Right),
    'J' -> Set(Up, Left),
    '|' -> Set(Up, Down),
    '-' -> Set(Left, Right),
    'S' -> Set(Up, Down, Left, Right),
    '.' -> Set.empty,
)

def parseMap(input: List[String]): Grid = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Point(y, x) -> ch).toMap
}

def loopPositions(map: Grid): (Set[Point], Char) = {
    var position = map.keys.find(k => map(k) == 'S').get
    var positions = Set.empty[Point]

    val initialDir = Dirs.find(dir => {
        val nextPos = position + dir
        map.contains(nextPos) && Exits(map(nextPos)).contains(-dir)
    }).get

    var dir = initialDir
    var sReplacement = ' '

    breakable {
        while (true) {
            positions += position
            position += dir

            if (map(position) == 'S') {
                // Determine the replacement for 'S' based on the first and last directions
                val lastDir = -dir
                sReplacement = (initialDir, lastDir) match {
                    case (Up, Down) | (Down, Up) => '|'
                    case (Left, Right) | (Right, Left) => '-'
                    case (Up, Right) | (Right, Up) => 'L'
                    case (Up, Left) | (Left, Up) => 'J'
                    case (Down, Left) | (Left, Down) => '7'
                    case (Down, Right) | (Right, Down) => 'F'
                    case _ => 'S' // Fallback, though this case shouldn't occur
                }

                break()
            }

            dir = Exits(map(position)).find(_ != -dir).get
        }
    }

    return (positions, sReplacement)
}

// Check if position is inside the loop using ray casting algorithm
def inside(positionInit: Point, map: Grid, loop: Set[Point]): Boolean = {
    // Imagine a small elf starting from the top half of a cell and moving 
    // to the left jumping over the pipes it encounters. It needs to jump 
    // over only 'vertically' oriented pipes leading upwards, since it runs 
    // in the top of the row. Each jump flips the "inside" variable.

    if (loop.contains(positionInit)) return false

    var inside = false
    var position = positionInit + Left

    while (map.contains(position)) {
        if (loop.contains(position) && Exits(map(position)).contains(Up)) {
            inside = !inside
        }
        
        position += Left
    }

    return inside
}

def solver(input: List[String]) = {
    val originalMap = parseMap(input)
    val (loop, sReplacement) = loopPositions(originalMap)
    val startPos = originalMap.keys.find(k => originalMap(k) == 'S').get
    val updatedMap = originalMap.updated(startPos, sReplacement)

    println(s"Part One: ${loop.size / 2}")

    val partTwo = updatedMap.keys.count(inside(_, updatedMap, loop))
    println(s"Part Two: $partTwo")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day10.txt") match {
        case Success(lines) => solver(lines)
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}