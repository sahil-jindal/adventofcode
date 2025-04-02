package day12

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Complex(re: Int, im: Int) {
    def +(that: Complex): Complex = Complex(re + that.re, im + that.im)
    def -(that: Complex): Complex = Complex(re - that.re, im - that.im)
    def *(scalar: Int): Complex = Complex(re * scalar, im * scalar)
    def *(that: Complex): Complex = Complex(re * that.re - im * that.im, re * that.im + im * that.re)
}

case class State(pos: Complex, dir: Complex)

def moveShip(input: List[String], partOne: Boolean): Int = {
    val initialState = State(
        pos = Complex(0, 0),
        dir = if (partOne) Complex(1, 0) else Complex(10, 1)
    )

    val finalState = input.map { line =>
        (line.head, line.tail.toInt)
    }.foldLeft(initialState) { case (state, (action, arg)) =>
        (action, arg) match {
            case ('N', value) if partOne => state.copy(pos = state.pos + Complex(0, value))
            case ('N', value)            => state.copy(dir = state.dir + Complex(0, value))
            case ('S', value) if partOne => state.copy(pos = state.pos - Complex(0, value))
            case ('S', value)            => state.copy(dir = state.dir - Complex(0, value))
            case ('E', value) if partOne => state.copy(pos = state.pos + Complex(value, 0))
            case ('E', value)            => state.copy(dir = state.dir + Complex(value, 0))
            case ('W', value) if partOne => state.copy(pos = state.pos - Complex(value, 0))
            case ('W', value)            => state.copy(dir = state.dir - Complex(value, 0))
            case ('F', value)            => state.copy(pos = state.pos + state.dir * value)
            case ('L', 90)  | ('R', 270) => state.copy(dir = state.dir * Complex(0, 1))
            case ('L', 270) | ('R', 90)  => state.copy(dir = state.dir * Complex(0, -1))
            case ('L', 180) | ('R', 180) => state.copy(dir = state.dir * Complex(-1, 0))
            case _ => throw new Exception("Invalid input")
        }
    }

    return finalState.pos.re.abs + finalState.pos.im.abs
}

def evaluatorOne(input: List[String]): Int = moveShip(input, true)
def evaluatorTwo(input: List[String]): Int = moveShip(input, false)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day12.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}