package day23

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{ListBuffer, Map, Set => MutableSet}

case class Direction(dy: Int, dx: Int) {
    def +(that: Direction) = Direction(dy + that.dy, dx + that.dx)
}

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

val N = Direction(-1, 0)
val E = Direction(0, 1)
val S = Direction(1, 0)
val W = Direction(0, -1)
val NW = N + W
val NE = N + E
val SE = S + E
val SW = S + W

val directions = Seq(NW, N, NE, E, SE, S, SW, W)

def parseInput(input: List[String]): Set[Point] = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
        if ch == '#'
    } yield Point(y, x)).toSet
}

def extendDir(dir: Direction): Seq[Direction] = {
    if dir == N then return Seq(NW, N, NE)
    if dir == E then return Seq(NE, E, SE)
    if dir == S then return Seq(SW, S, SE)
    if dir == W then return Seq(NW, W, SW)
    throw Exception()
}

def simulate(elvesInit: Set[Point]): List[Set[Point]] = {
    val elves = MutableSet(elvesInit.toSeq*)
    val result = ListBuffer.empty[Set[Point]]

    val lookAround = Iterator.continually(Seq(
        Seq(N, S, W, E), Seq(S, W, E, N), Seq(W, E, N, S), Seq(E, N, S, W)
    )).flatten

    var fixpoint = false

    while (!fixpoint) {
        val proposals = Map.empty[Point, ListBuffer[Point]]
        val orderofDirections = lookAround.next()

        for (elf <- elves; if directions.exists(dir => elves.contains(elf + dir))) {
            val proposes = orderofDirections.find(dir => extendDir(dir).forall(d => !elves.contains(elf + d)))

            if (proposes.isDefined) {
                val pos = elf + proposes.get
                proposals.getOrElseUpdate(pos, ListBuffer.empty) += elf
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

def area(elves: Set[Point]): Int = {
    // smallest enclosing rectangle
    val xs = elves.map(_.x)
    val ys = elves.map(_.y)

    val width = xs.max - xs.min + 1
    val height = ys.max - ys.min + 1

    return width * height - elves.size
}

def evaluatorOne(input: List[Set[Point]]): Int = area(input(9))
def evaluatorTwo(input: List[Set[Point]]): Int = input.size

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