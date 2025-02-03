package day05

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def getStepCount(lines: List[String], update: Int => Int): Int = {
    var input = lines.map(_.toInt).toArray
    var i = 0
    var stepCount = 0

    while(i >= 0 && i < input.length) {
        val jmp = input(i)
        input(i) = update(input(i))
        i += jmp
        stepCount += 1
    }

    return stepCount
}

def evaluatorOne(input: List[String]) = getStepCount(input, it => it + 1)
def evaluatorTwo(input: List[String]) = getStepCount(input, it => if it < 3 then it + 1 else it - 1 )

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

@main
def hello(): Unit =
    readLinesFromFile("day05.txt") match
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
