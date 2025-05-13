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

def iterate(input: List[String], iterations: Int, update: Virus => Virus): Int = {
    val height = input.length
    val width = input.head.length
    val cells = Map.empty[Point, State]

    for (yT <- 0 until height; xT <- 0 until width; if input(yT)(xT) == '#') do {
        cells += Point(yT, xT) -> State.Infected
    }

    var pos = Point(height / 2, width / 2)
    var dir = Direction(-1, 0)
    var infections = 0

    for(_ <- 0 until iterations) {
        var state = cells.getOrElse(pos, State.Clean)
        val newVirus = update(Virus(dir, state))

        state = newVirus.state
        dir = newVirus.currDirection
        
        if(state == State.Infected) infections += 1

        if(state == State.Clean) {
            cells -= pos
        } else {
            cells += pos -> state
        }

        pos += dir
    } 

    return infections
}

def evaluatorOne(input: List[String]) = iterate(input, 10000, it => {
    val Virus(dir, state) = it

    state match {
        case State.Clean => Virus(dir.rotateLeft, State.Infected)
        case State.Infected => Virus(dir.rotateRight, State.Clean)
        case _ => ??? // Purposely left as this returns "Nothing"
    }
})

def evaluatorTwo(input: List[String]) = iterate(input, 10000000, it => {
    val Virus(dir, state) = it

    state match {
        case State.Flagged => Virus(-dir, State.Clean)
        case State.Weakened => Virus(dir, State.Infected)
        case State.Clean => Virus(dir.rotateLeft, State.Weakened)
        case State.Infected => Virus(dir.rotateRight, State.Flagged)       
    }
})

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day22.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}