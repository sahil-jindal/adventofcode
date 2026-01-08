package day02

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

sealed trait Input
case class Up(num: Int) extends Input
case class Down(num: Int) extends Input
case class Forward(num: Int) extends Input

case class State1(x: Int, y: Int)
case class State2(x: Int, y: Int, aim: Int)

def parseInput(input: List[String]) = input.collect {
    case s"up $num" => Up(num.toInt)
    case s"down $num" => Down(num.toInt)
    case s"forward $num" => Forward(num.toInt)
}

def evaluatorOne(input: List[Input]): Int = {
    val initialState = State1(0, 0)

    val res = input.foldLeft(initialState) { case (State1(x, y), step) =>
        step match {
            case Up(amount) => State1(x, y - amount)
            case Down(amount) => State1(x, y + amount)    
            case Forward(amount) => State1(x + amount, y)
        }
    }

    return res.x * res.y
}

def evaluatorTwo(input: List[Input]): Int = {
    val initialState = State2(0, 0, 0)

    val res = input.foldLeft(initialState) { case (State2(x, y, aim), step) =>
        step match {
            case Up(amount) => State2(x, y, aim - amount)
            case Down(amount) => State2(x, y, aim + amount)    
            case Forward(amount) => State2(x + amount, y + amount * aim, aim)
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