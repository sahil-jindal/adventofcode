package day17

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

val totalLiters = 150

def parseInput(input: List[String]) = input.map(_.toInt).sorted

def findAllCombinations(containers: List[Int], target: Int): List[List[Int]] = {
    if (target < 0) return Nil
    if (target == 0) return List(Nil)
    if (containers.isEmpty) return Nil
    
    val (curr, rest) = (containers.head, containers.tail)

    findAllCombinations(rest, target - curr).map(curr :: _) :::
    findAllCombinations(rest, target)
}

def solver(containers: List[Int]): (Int, Int) = {
    val combinations = findAllCombinations(containers, totalLiters)
    val minContainerCount = combinations.map(_.size).min
    return (combinations.size, combinations.count(_.size == minContainerCount))
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day17.txt") match {
        case Success(lines) => {
            val (partOne, partTwo) = solver(parseInput(lines))
            println(s"Part One: $partOne")
            println(s"Part Two: $partTwo")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}