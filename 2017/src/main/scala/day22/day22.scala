package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

enum State { case Clean, Weakened, Infected, Flagged }
case class Point(val y: Int, val x: Int)
case class Direction(val dy: Int, val dx: Int)
case class Virus(val currDirection: Direction, val state: State)

def iterate(lines: List[String], iterations: Int, update: Virus => Virus): Int = {
    val height = lines.length
    val width = lines.head.length
    val cells = Map.empty[Point, State]

    for (yT <- 0 until height; xT <- 0 until width; if lines(yT)(xT) == '#') do {
        cells += Point(yT, xT) -> State.Infected
    }

    var (y, x) = (height / 2, width / 2)
    var (dy, dx) = (-1, 0)
    var infections = 0

    for(_ <- 0 until iterations) {
        var state = cells.getOrElse(Point(y, x), State.Clean)
        val newVirus = update(Virus(Direction(dy, dx), state))

        state = newVirus.state
        dy = newVirus.currDirection.dy
        dx = newVirus.currDirection.dx

        if(state == State.Infected) infections += 1

        if(state == State.Clean) {
            cells -= Point(y, x)
        } else {
            cells += Point(y, x) -> state
        }

        y += dy
        x += dx
    } 

    return infections
}

def evaluatorOne(input: List[String]) = iterate(input, 10000, it => {
    val Virus(Direction(dy, dx), state) = it

    state match {
        case State.Clean => Virus(Direction(-dx, dy), State.Infected)
        case State.Infected => Virus(Direction(dx, -dy), State.Clean)
        case _ => ??? // Purposely left as this returns "Nothing"
    }
})

def evaluatorTwo(input: List[String]) = iterate(input, 10000000, it => {
    val Virus(Direction(dy, dx), state) = it

    state match {
        case State.Clean => Virus(Direction(-dx, dy), State.Weakened)
        case State.Weakened => Virus(Direction(dy, dx), State.Infected)
        case State.Infected => Virus(Direction(dx, -dy), State.Flagged)       
        case State.Flagged => Virus(Direction(-dy, -dx), State.Clean)
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