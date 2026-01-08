package day01

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Step(pos: Int, zeroHits: Int)

def parseInput(input: List[String]) = input.collect {
    case s"L$amount" => -amount.toInt
    case s"R$amount" =>  amount.toInt
}

def preComputation(numbers: List[Int]): List[Step] = {
    return numbers.scanLeft(Step(50, 0)) { (prev, move) =>
        val start = prev.pos
        val absDist = move.abs
        val raw = start + move
        val end = ((raw % 100) + 100) % 100

        // compute first k (1..100) when we hit zero during this rotation
        val k0raw = if (move >= 0) ((100 - start) % 100) else (start % 100)
        val k0 = if (k0raw == 0) 100 else k0raw

        val passes = if (absDist < k0) 0 else 1 + (absDist - k0) / 100

        Step(end, passes)
    }
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