package day06

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Set

case class Direction(dy: Int, dx: Int) {
    def rotateRight = Direction(dx, -dy)
}

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

case class PairOne(pos: Point, dir: Direction)
case class PairTwo(positions: Set[Point], isLoop: Boolean)

type Grid = Map[Point, Char]

def parseInput(input: List[String]): (Grid, Point) = {
    val grid = (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Point(y, x) -> ch).toMap

    val start = grid.collectFirst { case (pt, ch) if ch == '^' => pt }.get

    return (grid, start)
}

def walker(grid: Grid, posInit: Point): PairTwo = {
    val seen = Set.empty[PairOne]

    var pos = posInit
    var dir = Direction(-1, 0)

    while (grid.contains(pos) && !seen.contains(PairOne(pos, dir))) {
        seen += PairOne(pos, dir)

        if (grid.getOrElse(pos + dir, ' ') == '#') {
            dir = dir.rotateRight
        } else {
            pos += dir
        }
    }

    return PairTwo(
        seen.map(_.pos), 
        seen.contains(PairOne(pos, dir))
    )
}

def solver(input: List[String]) = {
    val (grid, start) = parseInput(input)

    val positions = walker(grid, start).positions

    println(s"Part One: ${positions.size}")

    val count = positions.count(pos => {
        walker(grid.updated(pos, '#'), start).isLoop
    })

    println(s"Part Two: $count")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day06.txt") match {
        case Success(lines) => solver(lines)
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}