package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

enum State { case Clean, Weakened, Infected, Flagged }

case class Direction(dy: Int, dx: Int) {
    def unary_- = Direction(-dy, -dx)
    def rotateLeft = Direction(-dx, dy)
    def rotateRight = Direction(dx, -dy)
}

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

case class Virus(currDirection: Direction, state: State)

def parseInput(input: List[String]): List[Point] = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
        if ch == '#'
    } yield Point(y, x)).toList
}

def iterate(input: List[Point], iterations: Int, update: PartialFunction[Virus, Virus]): Int = {
    val cells = Map.from(input.map(_ -> State.Infected))

    var pos = Point(input.map(_.y).max / 2, input.map(_.x).max / 2)
    var dir = Direction(-1, 0)
    var infections = 0

    for (_ <- 0 until iterations) {
        var state = cells.getOrElse(pos, State.Clean)
        val newVirus = update(Virus(dir, state))

        state = newVirus.state
        dir = newVirus.currDirection
        
        if (state == State.Infected) infections += 1

        if (state == State.Clean) {
            cells -= pos
        } else {
            cells += pos -> state
        }

        pos += dir
    } 

    return infections
}

def evaluatorOne(input: List[Point]) = iterate(input, 10000, {
    case Virus(dir, State.Clean) => Virus(dir.rotateLeft, State.Infected)
    case Virus(dir, State.Infected) => Virus(dir.rotateRight, State.Clean)
})

def evaluatorTwo(input: List[Point]) = iterate(input, 10000000, { 
    case Virus(dir, State.Flagged) => Virus(-dir, State.Clean)
    case Virus(dir, State.Weakened) => Virus(dir, State.Infected)
    case Virus(dir, State.Clean) => Virus(dir.rotateLeft, State.Weakened)
    case Virus(dir, State.Infected) => Virus(dir.rotateRight, State.Flagged)
})

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day22.txt") match {
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