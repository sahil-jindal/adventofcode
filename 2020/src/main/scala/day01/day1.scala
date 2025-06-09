package day01

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: List[String]) = input.map(_.toInt).toSet

def partitions(total: Int, n: Int): List[List[Int]] = {
    def helper(remaining: Int, length: Int, minValue: Int): List[List[Int]] = {
        if (length <= 0 || length > remaining) return List(Nil)
        if (length == 1) return List(List(remaining))

        val maxValue = remaining / length

        return (minValue to maxValue).toList.flatMap { value =>
            helper(remaining - value, length - 1, value).map(value :: _)
        }
    }

    return helper(total, n, 1)
}

def solver(numbers: Set[Int], n: Int): Int = {
    return partitions(2020, n).find(_.forall(numbers.contains)).get.product
}

def evaluatorOne(numbers: Set[Int]): Int = solver(numbers, 2)
def evaluatorTwo(numbers: Set[Int]): Int = solver(numbers, 3)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day01.txt") match {
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