package day01

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer

case class Pair(first: List[Int], second: List[Int])

def parseInput(input: List[String]): Pair = {
    val num1 = ListBuffer.empty[Int]
    val num2 = ListBuffer.empty[Int]

    for (line <- input) {
        val nums = line.split("   ").map(_.toInt)
        num1 += nums(0)
        num2 += nums(1)
    }

    return Pair(num1.sorted.toList, num2.sorted.toList)
}

def evaluatorOne(input: Pair): Int = {
    val Pair(first, second) = input
    return (first zip second).map { case (a, b) => (a - b).abs }.sum
}

def evaluatorTwo(input: Pair): Int = {
    val Pair(first, second) = input
    val weights = second.groupMapReduce(identity)(_ => 1)(_ + _)

    return first.map(num => weights.getOrElse(num, 0) * num).sum
}

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