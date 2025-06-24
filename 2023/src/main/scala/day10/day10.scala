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

def loopPositions(grid: Grid): (Set[Point], Grid) = {
    var position = grid.collectFirst { case (k, v) if v == 'S' => k }.get
    val positions = Set.empty[Point]

    val initialDir = Dirs.find(dir => {
        val nextPos = position + dir
        grid.contains(nextPos) && Exits(grid(nextPos)).contains(-dir)
    }).get

    var dir = initialDir

    breakable {
        while (true) {
            positions += position
            position += dir

            if (grid(position) == 'S') break()
            
            dir = Exits(grid(position)).find(_ != -dir).get
        }
    }

    // Determine the replacement for 'S' based on the first and last directions
    val lastDir = -dir

    val sReplacement = (initialDir, lastDir) match {
        case (Up, Down) | (Down, Up) => '|'
        case (Left, Right) | (Right, Left) => '-'
        case (Up, Right) | (Right, Up) => 'L'
        case (Up, Left) | (Left, Up) => 'J'
        case (Down, Left) | (Left, Down) => '7'
        case (Down, Right) | (Right, Down) => 'F'
        case _ => 'S' // Fallback, though this case shouldn't occur
    }

    return (positions, grid.updated(position, sReplacement))
}

// Check if position is inside the loop using ray casting algorithm
def inside(positionInit: Point, grid: Grid, loop: Set[Point]): Boolean = {
    // Imagine a small elf starting from the top half of a cell and moving 
    // to the left jumping over the pipes it encounters. It needs to jump 
    // over only 'vertically' oriented pipes leading upwards, since it runs 
    // in the top of the row. Each jump flips the "inside" variable.

    if (loop.contains(positionInit)) return false

    var inside = false
    var position = positionInit + Left

    while (grid.contains(position)) {
        if (loop.contains(position) && Exits(grid(position)).contains(Up)) {
            inside = !inside
        }
        
        position += Left
    }

    return inside
}

def solver(input: List[String]): (Int, Int) = {
    val originalMap = parseMap(input)
    val (loop, updatedMap) = loopPositions(originalMap)
    return (loop.size / 2, updatedMap.keys.count(inside(_, updatedMap, loop)))
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day10.txt") match {
        case Success(lines) => {
            val (partOne, partTwo) = solver(lines)
            println(s"Part One: $partOne")
            println(s"Part Two: $partTwo")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}