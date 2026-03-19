package day06

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.immutable.Range.Inclusive
import scala.reflect.ClassTag

enum Actions { case Toggle, TurnOn, TurnOff }

case class Instruction(action: Actions, xRange: Inclusive, yRange: Inclusive)

trait LightGrid[A] {
    def initial: A
    def interpret(action: Actions)(cell: A): A
    def aggregate(grid: Array[Array[A]]): Int
}

given LightGrid[Boolean] with {
    def initial = false
    
    def interpret(action: Actions)(cell: Boolean) = action match {
        case Actions.Toggle  => !cell
        case Actions.TurnOn  => true
        case Actions.TurnOff => false
    }

    def aggregate(grid: Array[Array[Boolean]]): Int = {
        return grid.flatten.count(identity) 
    }
}

given LightGrid[Int] with {
    def initial = 0
    
    def interpret(action: Actions)(cell: Int) = action match {
        case Actions.Toggle  => cell + 2
        case Actions.TurnOn  => cell + 1
        case Actions.TurnOff => (cell - 1).max(0)
    }

    def aggregate(grid: Array[Array[Int]]): Int = {
        return grid.flatten.sum
    }
}

val toggleRegex = raw"toggle (\d+),(\d+) through (\d+),(\d+)".r
val turnOnRegex = raw"turn on (\d+),(\d+) through (\d+),(\d+)".r
val turnOffRegex = raw"turn off (\d+),(\d+) through (\d+),(\d+)".r

def parseInput(input: List[String]) = input.collect {
    case toggleRegex(a, b, c, d) => Instruction(Actions.Toggle, a.toInt to c.toInt, b.toInt to d.toInt)
    case turnOnRegex(a, b, c, d) => Instruction(Actions.TurnOn, a.toInt to c.toInt, b.toInt to d.toInt)
    case turnOffRegex(a, b, c, d) => Instruction(Actions.TurnOff, a.toInt to c.toInt, b.toInt to d.toInt)
}

def solver[A: ClassTag](input: List[Instruction])(using lg: LightGrid[A]): Int = {
    val grid = Array.fill(1000, 1000)(lg.initial)
    
    for (Instruction(action, xRange, yRange) <- input) {
        val update = lg.interpret(action)
        
        for (i <- yRange; j <- xRange) {
            grid(i)(j) = update(grid(i)(j))
        }
    }

    return lg.aggregate(grid)
}

def evaluatorOne(input: List[Instruction]): Int = solver[Boolean](input)
def evaluatorTwo(input: List[Instruction]): Int = solver[Int](input)

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