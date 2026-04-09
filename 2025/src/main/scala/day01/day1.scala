package day01

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

// Computes both parts together. Part two left (or negative) turns are easier to handle
// if we first "reverse" the dial, then treat it as a right turn. The [`rem_euclid`] method
// is a modulo operator that handles negative values. For example `-1.rem_euclid(100)` is 99.

case class Step(pos: Int, zeroHits: Int)

sealed trait Move { def rotate(dial: Int): Step }

case class Left(amount: Int) extends Move {
    override def rotate(dial: Int): Step = {
        val newDial = dial + amount
        return Step(newDial % 100, newDial / 100)
    }
}

case class Right(amount: Int) extends Move {
    override def rotate(dial: Int): Step = {
        val reversed = (100 - dial) % 100
        val pos = Math.floorMod(dial - amount, 100)
        val zeroHits = (reversed + amount) / 100
        return Step(pos, zeroHits)
    }
}

def parseInput(input: List[String]) = input.collect {
    case s"L$amount" => Left(amount.toInt)
    case s"R$amount" => Right(amount.toInt)
}

def preComputation(moves: List[Move]): List[Step] = {
    return moves.scanLeft(Step(50, 0)) { (prev, move) => move.rotate(prev.pos) }
} 

def evaluatorOne(steps: List[Step]) = steps.count(_.pos == 0)
def evaluatorTwo(steps: List[Step]) = steps.map(_.zeroHits).sum

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day01.txt") match {
        case Success(lines) => {
            val input = preComputation(parseInput(lines))
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}