package day06

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parse(input: String): List[Long] = {
    return raw"(\d+)".r.findAllIn(input).map(_.toLong).toList
}

// solves ax^2 + bx + c = 0 (supposing two different roots)
def solveEq(a: Double, b: Double, c: Double): (Double, Double) = {
    val d = Math.sqrt(b * b - 4 * a * c)
    val x1 = (-b - d) / (2 * a)
    val x2 = (-b + d) / (2 * a)
    return (x1, x2)
}

def winningMoves(time: Long, record: Long): Long = {
    // If we wait x ms, our boat moves `(time - x) * x` millimeters.
    // This breaks the record when `(time - x) * x > record`
    // or `-x^2  + time * x - record > 0`.

    // get the roots first
    val (x1, x2) = solveEq(-1, time.toDouble, -record.toDouble)

    // For this type of equations, x1 >= x2 as long as time and record are +ve
    // integers in between the roots
    val minX = x2.floor.toLong + 1
    val maxX = x1.ceil.toLong - 1

    return maxX - minX + 1
}

def solve(input: List[String], converter: String => String): Long = {
    val times = parse(converter(input(0)))
    val records = parse(converter(input(1)))

    return (times zip records).map(winningMoves).product
}

def evaluatorOne(input: List[String]): Long = solve(input, identity)
def evaluatorTwo(input: List[String]): Long = solve(input, it => it.replaceAll(" ", ""))

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day06.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}