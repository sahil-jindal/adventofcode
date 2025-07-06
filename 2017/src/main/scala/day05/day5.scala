package day05

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: List[String]) = input.map(_.toInt)

def getStepCount(inputInit: List[Int], update: Int => Int): Int = {
    var input = inputInit.toArray
    var i = 0
    var stepCount = 0

    while (i >= 0 && i < input.length) {
        val jmp = input(i)
        input(i) = update(input(i))
        i += jmp
        stepCount += 1
    }

    return stepCount
}

def evaluatorOne(input: List[Int]): Int = getStepCount(input, it => it + 1)
def evaluatorTwo(input: List[Int]): Int = getStepCount(input, it => if it < 3 then it + 1 else it - 1 )

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day05.txt") match {
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