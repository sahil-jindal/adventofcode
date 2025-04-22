package day23

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable

case class Complex(real: Int, imaginary: Int) {
    def +(that: Complex) = Complex(real + that.real, imaginary + that.imaginary)
}

val N = Complex(0, -1)
val E = Complex(1, 0)
val S = Complex(0, 1)
val W = Complex(-1, 0)
val NW = N + W
val NE = N + E
val SE = S + E
val SW = S + W

val directions = List(NW, N, NE, E, SE, S, SW, W)

def parseInput(input: List[String]): Set[Complex] = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
        if ch == '#'
    } yield Complex(x, y)).toSet
}

def extendDir(dir: Complex): Seq[Complex] = {
    if dir == N then return Seq(NW, N, NE)
    if dir == E then return Seq(NE, E, SE)
    if dir == S then return Seq(SW, S, SE)
    if dir == W then return Seq(NW, W, SW)
    throw Exception()
}

def simulate(elvesInit: Set[Complex]): List[Set[Complex]] = {
    val elves = mutable.Set(elvesInit.toSeq*)
    val result = mutable.ListBuffer.empty[Set[Complex]]

    val lookAround = Iterator.continually(List(
        List(N, S, W, E), List(S, W, E, N), List(W, E, N, S), List(E, N, S, W)
    )).flatten

    var fixpoint = false

    while (!fixpoint) {
        val proposals = mutable.Map.empty[Complex, mutable.ListBuffer[Complex]]
        val orderofDirections = lookAround.next()

        for (elf <- elves; if directions.exists(dir => elves.contains(elf + dir))) {
            val proposes = orderofDirections.find(dir => extendDir(dir).forall(d => !elves.contains(elf + d)))

            if (proposes.isDefined) {
                val pos = elf + proposes.get
                proposals.getOrElseUpdate(pos, mutable.ListBuffer.empty) += elf
            }
        }

        fixpoint = true
        
        for ((to, from) <- proposals) {
            if (from.size == 1) {
                elves.remove(from.head)
                elves.add(to)
                fixpoint = false
            }
        }

        result += elves.toSet
    }

    return result.toList
}

def area(elves: Set[Complex]): Int = {
    // smallest enclosing rectangle
    val xs = elves.map(_.real)
    val ys = elves.map(_.imaginary)

    val width = xs.max - xs.min + 1
    val height = ys.max - ys.min + 1

    return width * height - elves.size
}

def evaluatorOne(input: List[Set[Complex]]): Int = area(input(9))
def evaluatorTwo(input: List[Set[Complex]]): Int = input.size

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day23.txt") match {
        case Success(lines) => {
            val input = simulate(parseInput(lines))
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}