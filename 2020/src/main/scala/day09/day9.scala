package day09

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: List[String]) = input.map(_.toLong)

def findFirstInvalidNumber(numbers: List[Long], preambleSize: Int): Option[Long] = {
    return numbers.sliding(preambleSize + 1).collectFirst { 
        case (preamble :+ target) if preamble.combinations(2).forall(_.sum != target) => target  
    }
}

def findEncryptionWeakness(numbers: List[Long], target: Long): Option[Long] = {
    return (2 to numbers.length).iterator.flatMap(numbers.sliding).collectFirst {
        case contiguousSet if contiguousSet.sum == target => contiguousSet.min + contiguousSet.max
    }
}

def solver(input: List[Long]): (Long, Long) = {
    val preambleSize = 25
    val invalidNumber = findFirstInvalidNumber(input, preambleSize).get
    val encryptionWeakness = findEncryptionWeakness(input, invalidNumber).get
    return (invalidNumber, encryptionWeakness)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day09.txt") match {
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