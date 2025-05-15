package day09

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: List[String]) = input.map(_.toLong)

def findFirstInvalidNumber(numbers: List[Long], preambleSize: Int): Option[Long] = {
    numbers.sliding(preambleSize + 1).find { window =>
        val preamble = window.take(preambleSize)
        val target = window.last
        
        // Check if target can be formed by sum of any two distinct numbers in preamble
        !preamble.combinations(2).exists(_.sum == target)
    }
    .map(_.last)
}

def findContiguousSet(numbers: List[Long], target: Long): Option[Long] = {
    // Try all possible starting points
    numbers.indices.flatMap { start =>
        // Find the smallest contiguous set that sums to target
        (start + 1 until numbers.length).find { end =>
            val contiguousSet = numbers.slice(start, end)
            contiguousSet.sum == target
        }.map { end =>
            val contiguousSet = numbers.slice(start, end)
            contiguousSet.min + contiguousSet.max
        }
    }.headOption
}

def solve(input: List[Long]) = {
     // Part 1: Find first number that doesn't follow the rule
    val preambleSize = 25
    val invalidNumber = findFirstInvalidNumber(input, preambleSize).get
    println(s"Part 1 - First invalid number: ${invalidNumber}")
    
    // Part 2: Find encryption weakness
    val encryptionWeakness = findContiguousSet(input, invalidNumber).get
    println(s"Part 2 - Encryption weakness: ${encryptionWeakness}")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day09.txt") match {
        case Success(lines) => solve(parseInput(lines))
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}