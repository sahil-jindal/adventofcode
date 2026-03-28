package day02

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

sealed trait Input
case class Up(num: Int) extends Input
case class Down(num: Int) extends Input
case class Forward(num: Int) extends Input

def parseInput(input: List[String]) = input.collect {
    case s"up $num" => Up(num.toInt)
    case s"down $num" => Down(num.toInt)
    case s"forward $num" => Forward(num.toInt)
}

def evaluatorOne(input: List[Input]): Int = {
    var (x, y) = (0, 0)

    for (step <- input) {
        step match {
            case Up(amount) => y -= amount
            case Down(amount) => y += amount   
            case Forward(amount) => x += amount
        }
    }

    return x * y
}

def evaluatorTwo(input: List[Input]): Int = {
    var (x, y, aim) = (0, 0, 0)

    for (step <- input) {
        step match {
            case Up(amount) => aim -= amount
            case Down(amount) => aim += amount    
            case Forward(amount) => x += amount; y += amount * aim
        }
    }

    return x * y
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