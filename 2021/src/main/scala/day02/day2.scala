package day02

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Input(dir: Char, amount: Int)
case class State1(x: Int, y: Int)
case class State2(x: Int, y: Int, aim: Int)

def parseInput(input: List[String]) = input.map(line => {
    val Array(part1, part2) = line.split(" ")
    Input(part1.head, part2.toInt)
})

def evaluatorOne(input: List[Input]): Int = {
    val initialState = State1(0, 0)

    val res = input.foldLeft(initialState) { case (state, step) =>
        step.dir match {
            case 'f' => state.copy(x = state.x + step.amount)
            case 'u' => state.copy(y = state.y - step.amount)
            case 'd' => state.copy(y = state.y + step.amount)    
        }
    }

    return res.x * res.y
}

def evaluatorTwo(input: List[Input]): Int = {
    val initialState = State2(0, 0, 0)

    val res = input.foldLeft(initialState) { case (state, step) =>
        step.dir match {
            case 'f' => state.copy(x = state.x + step.amount, y = state.y + step.amount * state.aim)
            case 'u' => state.copy(aim = state.aim - step.amount)
            case 'd' => state.copy(aim = state.aim + step.amount)    
        }
    }

    return res.x * res.y
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day02.txt") match {
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