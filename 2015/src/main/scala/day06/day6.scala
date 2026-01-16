package day06

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.immutable.Range.Inclusive

enum Actions { case Toggle, TurnOn, TurnOff }

case class Instruction(action: Actions, xRange: Inclusive, yRange: Inclusive)

val toggleRegex = raw"toggle (\d+),(\d+) through (\d+),(\d+)".r
val turnOnRegex = raw"turn on (\d+),(\d+) through (\d+),(\d+)".r
val turnOffRegex = raw"turn off (\d+),(\d+) through (\d+),(\d+)".r

def parseInput(input: List[String]) = input.collect {
    case toggleRegex(a, b, c, d) => Instruction(Actions.Toggle, a.toInt to c.toInt, b.toInt to d.toInt)
    case turnOnRegex(a, b, c, d) => Instruction(Actions.TurnOn, a.toInt to c.toInt, b.toInt to d.toInt)
    case turnOffRegex(a, b, c, d) => Instruction(Actions.TurnOff, a.toInt to c.toInt, b.toInt to d.toInt)
}

def evaluatorOne(input: List[Instruction]): Int = {
    
    def turnOnBrightness(grid: Array[Array[Boolean]], xRange: Inclusive, yRange: Inclusive) = {
        for { i <- yRange; j <- xRange } do grid(i)(j) = true
    }

    def turnOffBrightness(grid: Array[Array[Boolean]], xRange: Inclusive, yRange: Inclusive) = {
        for { i <- yRange; j <- xRange } do grid(i)(j) = false
    }

    def toggleBrightness(grid: Array[Array[Boolean]], xRange: Inclusive, yRange: Inclusive) = {
        for { i <- yRange; j <- xRange } do grid(i)(j) = !grid(i)(j)
    }
    
    val grid = Array.ofDim[Boolean](1000, 1000)

    for (Instruction(action, xRange, yRange) <- input) {
        action match {
            case Actions.Toggle => toggleBrightness(grid, xRange, yRange)
            case Actions.TurnOn => turnOnBrightness(grid, xRange, yRange)
            case Actions.TurnOff => turnOffBrightness(grid, xRange, yRange)
        }
    }

    return grid.flatten.count(identity)
}

def evaluatorTwo(input: List[Instruction]): Int = {
    
    def turnOnBrightness(grid: Array[Array[Int]], xRange: Inclusive, yRange: Inclusive) = {
        for { i <- yRange; j <- xRange } do grid(i)(j) += 1
    }

    def turnOffBrightness(grid: Array[Array[Int]], xRange: Inclusive, yRange: Inclusive) = {
        for { i <- yRange; j <- xRange } do grid(i)(j) = math.max(0, grid(i)(j) - 1)
    }

    def toggleBrightness(grid: Array[Array[Int]], xRange: Inclusive, yRange: Inclusive) = {
        for { i <- yRange; j <- xRange } do grid(i)(j) += 2
    }
    
    val grid = Array.ofDim[Int](1000, 1000)

    for (Instruction(action, xRange, yRange) <- input) {
        action match {
            case Actions.Toggle => toggleBrightness(grid, xRange, yRange)
            case Actions.TurnOn => turnOnBrightness(grid, xRange, yRange)
            case Actions.TurnOff => turnOffBrightness(grid, xRange, yRange)
        }
    }

    return grid.flatten.sum
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day06.txt") match {
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