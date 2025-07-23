package day12

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Direction(dy: Int, dx: Int) {
    def unary_- = Direction(-dy, -dx)
    def rotateLeft = Direction(dx, -dy)
    def rotateRight = Direction(-dx, dy)
    def *(num: Int) = Direction(dy * num, dx * num)
    def +(dir: Direction) = Direction(dy + dir.dy, dx + dir.dx)
}

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

case class State(pos: Point, dir: Direction)
case class Pair(action: Char, arg: Int)

def parseInput(input: List[String]) = input.map(line => Pair(line.head, line.tail.toInt))

def moveShip(pairs: List[Pair], partOne: Boolean): Int = {
    val initialState = State(pos = Point(0, 0), dir = if (partOne) Direction(0, 1) else Direction(1, 10))
    
    val finalState = pairs.foldLeft(initialState) { case (state, Pair(action, arg)) =>
        (action, arg) match {
            case ('N', value) if partOne => state.copy(pos = state.pos + Direction(value, 0))
            case ('N', value)            => state.copy(dir = state.dir + Direction(value, 0))
            case ('S', value) if partOne => state.copy(pos = state.pos + Direction(-value, 0))
            case ('S', value)            => state.copy(dir = state.dir + Direction(-value, 0))
            case ('E', value) if partOne => state.copy(pos = state.pos + Direction(0, value))
            case ('E', value)            => state.copy(dir = state.dir + Direction(0, value))
            case ('W', value) if partOne => state.copy(pos = state.pos + Direction(0, -value))
            case ('W', value)            => state.copy(dir = state.dir + Direction(0, -value))
            case ('F', value)            => state.copy(pos = state.pos + state.dir * value)
            case ('L', 90)  | ('R', 270) => state.copy(dir = state.dir.rotateLeft)
            case ('L', 270) | ('R', 90)  => state.copy(dir = state.dir.rotateRight)
            case ('L', 180) | ('R', 180) => state.copy(dir = -state.dir)
            case _ => throw new Exception("Invalid input")
        }
    }

    return finalState.pos.x.abs + finalState.pos.y.abs
}

def evaluatorOne(input: List[Pair]): Int = moveShip(input, true)
def evaluatorTwo(input: List[Pair]): Int = moveShip(input, false)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day12.txt") match {
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