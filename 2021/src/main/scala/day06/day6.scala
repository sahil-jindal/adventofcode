package day06

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: String) = input.split(",").map(_.toInt).toList

def fishCountAfterNDays(fish: List[Int], days: Int): Long = {
    val fishCountByInternalTimer = Array.fill(9)(0L)

    for (timer <- fish) {
        fishCountByInternalTimer(timer) += 1
    }

    for (t <- 0 until days) {
        fishCountByInternalTimer((t + 7) % 9) += fishCountByInternalTimer(t % 9)
    }

    return fishCountByInternalTimer.sum
}

def evaluatorOne(input: List[Int]): Long = fishCountAfterNDays(input, 80)
def evaluatorTwo(input: List[Int]): Long = fishCountAfterNDays(input, 256)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day06.txt") match {
        case Success(lines) => {
            val input = parseInput(lines.head)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}