package day16

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, Set}

case class Complex(real: Int, imag: Int) {
    def +(that: Complex) = Complex(real + that.real, imag + that.imag)
}

case class Beam(pos: Complex, dir: Complex)

type Grid = Map[Complex, Char]

val Up = Complex(0, -1)
val Down = Complex(0, 1)
val Left = Complex(-1, 0)
val Right = Complex(1, 0)

def parseInput(input: List[String]): Grid = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Complex(x, y) -> ch).toMap
}

// the 'exit' direction(s) of the given cell when entered by a beam moving in 'dir'
// we have some special cases for mirrors and spliters, the rest keeps the direction
def exits(cell: Char, dir: Complex) = cell match {
    case '-' if dir == Up || dir == Down => Seq(Left, Right)
    case '|' if dir == Left || dir == Right => Seq(Up, Down)
    case '/' => Seq(Complex(-dir.imag, -dir.real))
    case '\\' => Seq(Complex(dir.imag, dir.real))
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
    val br = map.keys.maxBy(pos => pos.imag + pos.real)

    val a = map.keys.collect { case pos if pos.real == 0 => Beam(pos, Down) }.toSeq
    val b = map.keys.collect { case pos if pos.imag == 0 => Beam(pos, Right) }.toSeq
    val c = map.keys.collect { case pos if pos.imag == br.imag => Beam(pos, Up) }.toSeq
    val d = map.keys.collect { case pos if pos.real == br.real => Beam(pos, Left) }.toSeq
    
    return a ++ b ++ c ++ d
}

def evaluatorOne(input: Grid): Int = energizedCells(input, Beam(Complex(0, 0), Right))
def evaluatorTwo(input: Grid): Int = startBeams(input).map(it => energizedCells(input, it)).max

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