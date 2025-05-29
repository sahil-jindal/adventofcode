package day17

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

val totalLiters = 150

def parseInput(input: List[String]) = input.map(_.toInt).sorted

def findAllCombinations(containers: List[Int], target: Int): List[List[Int]] = {
    if (target < 0 || containers.isEmpty) return Nil
    if (target == 0) return List(Nil)               
    
    findAllCombinations(containers.tail, target - containers.head).map(containers.head :: _) :::
    findAllCombinations(containers.tail, target)
}

def evaluatorOne(containers: List[Int]): Int = findAllCombinations(containers, totalLiters).size

def evaluatorTwo(containers: List[Int]): Int = {
    val combinations = findAllCombinations(containers, totalLiters)
    val minContainerCount = combinations.map(_.size).min
    return combinations.count(_.size == minContainerCount)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day17.txt") match {
        case Success(lines) => {
            val containers = parseInput(lines)
            println(s"Part One: ${evaluatorOne(containers)}")
            println(s"Part Two: ${evaluatorTwo(containers)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}