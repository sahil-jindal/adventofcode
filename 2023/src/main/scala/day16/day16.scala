package day16

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, Set}

case class Direction(dy: Int, dx: Int) {
    def reflectNW = Direction(dx, dy)
    def reflectNE = Direction(-dx, -dy)
}

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

case class Beam(pos: Point, dir: Direction)

type Grid = Map[Point, Char]

val Up = Direction(-1, 0)
val Down = Direction(1, 0)
val Left = Direction(0, -1)
val Right = Direction(0, 1)

def parseInput(input: List[String]): Grid = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Point(y, x) -> ch).toMap
}

// the 'exit' direction(s) of the given cell when entered by a beam moving in 'dir'
// we have some special cases for mirrors and spliters, the rest keeps the direction
def exits(cell: Char, dir: Direction) = cell match {
    case '-' if dir.dx == 0 => Seq(Left, Right)
    case '|' if dir.dy == 0 => Seq(Up, Down)
    case '/' => Seq(dir.reflectNE)
    case '\\' => Seq(dir.reflectNW)
    case _ => Seq(dir)
}

// follow the beam in the map and return the energized cell count. 
// this is essentially just a flood fill algorithm.
def energizedCells(map: Grid, beam: Beam): Int = {
    val q = Queue(beam)
    val seen = Set(beam)

    while (q.nonEmpty) {
        val beam = q.dequeue()
        seen += beam

        for (dir <- exits(map(beam.pos), beam.dir)) {
            val pos = beam.pos + dir
            if (map.contains(pos) && !seen.contains(Beam(pos, dir))) {
                q.enqueue(Beam(pos, dir))
            }
        }
    }

    return seen.map(_.pos).size
}

// go around the edges (top, right, bottom, left order) of the map
// and return the inward pointing directions
def startBeams(map: Grid): Seq[Beam] = {
    val br = map.keys.maxBy(pos => pos.y + pos.x)

    val a = map.keys.collect { case pos if pos.x == 0 => Beam(pos, Down) }.toSeq
    val b = map.keys.collect { case pos if pos.y == 0 => Beam(pos, Right) }.toSeq
    val c = map.keys.collect { case pos if pos.y == br.y => Beam(pos, Up) }.toSeq
    val d = map.keys.collect { case pos if pos.x == br.x => Beam(pos, Left) }.toSeq
    
    return a ++ b ++ c ++ d
}

def evaluatorOne(input: Grid): Int = energizedCells(input, Beam(Point(0, 0), Right))
def evaluatorTwo(input: Grid): Int = startBeams(input).map(energizedCells(input, _)).max

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day16.txt") match {
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