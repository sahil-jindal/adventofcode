package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.HashMap

enum State { case Clean, Weakened, Infected, Flagged }
case class Point(val y: Int, val x: Int)
case class Direction(val dy: Int, val dx: Int)
case class Virus(val currDirection: Direction, val state: State)

def iterate(lines: List[String], iterations: Int, update: Virus => Virus) = {
    val crow = lines.length
    val ccol = lines(0).length
    val cells = HashMap[Point, State]()

    for (irowT <- 0 until crow; icolT <- 0 until ccol; if lines(irowT)(icolT) == '#') do {
        cells += Point(irowT, icolT) -> State.Infected
    }

    var (irow, icol) = (crow / 2, ccol / 2)
    var (dy, dx) = (-1, 0)
    var infections = 0

    for(_ <- 0 until iterations) {
        var state = cells.getOrElse(Point(irow, icol), State.Clean)
        val newVirus = update(Virus(Direction(dy, dx), state))

        state = newVirus.state
        dy = newVirus.currDirection.dy
        dx = newVirus.currDirection.dx

        if(state == State.Infected) infections += 1

        if(state == State.Clean) {
            cells -= Point(irow, icol)
        } else {
            cells += Point(irow, icol) -> state
        }

        irow += dy
        icol += dx
    } 

    infections
}

def evaluatorOne(input: List[String]) = iterate(input, 10000, it => {
    val Virus(Direction(drow, dcol), state) = it

    state match {
        case State.Clean => Virus(Direction(-dcol, drow), State.Infected)
        case State.Infected => Virus(Direction(dcol, -drow), State.Clean)
        case _ => ??? // Purposely left as this returns "Nothing"
    }
})

def evaluatorTwo(input: List[String]) = iterate(input, 10000000, it => {
    val Virus(Direction(drow, dcol), state) = it

    state match {
        case State.Clean => Virus(Direction(-dcol, drow), State.Weakened)
        case State.Weakened => Virus(Direction(drow, dcol), State.Infected)
        case State.Infected => Virus(Direction(dcol, -drow), State.Flagged)       
        case State.Flagged => Virus(Direction(-drow, -dcol), State.Clean)
    }
})

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

@main
def hello() =
    readLinesFromFile("day22.txt") match
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }