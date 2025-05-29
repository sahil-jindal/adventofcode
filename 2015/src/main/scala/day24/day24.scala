package day24

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: List[String]) = input.map(_.toLong).sorted

def findCombinations(numbers: List[Long], len: Int, target: Long): List[List[Long]] = {
    return numbers.combinations(len).filter(_.sum == target).toList
}

def solver(numbers: List[Long], groupLength: Int): Long = {
    val target = numbers.sum / groupLength

    val packages = (1 to numbers.length).collectFirst {
        case n if findCombinations(numbers, n, target).nonEmpty => 
            findCombinations(numbers, n, target)
    }

    return packages.get.map(_.product).min
}

def evaluatorOne(numbers: List[Long]): Long = solver(numbers, 3)
def evaluatorTwo(numbers: List[Long]): Long = solver(numbers, 4)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day24.txt") match {
        case Success(lines) => {
            val numbers = parseInput(lines)
            println(s"Part One: ${evaluatorOne(numbers)}")
            println(s"Part Two: ${evaluatorTwo(numbers)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}