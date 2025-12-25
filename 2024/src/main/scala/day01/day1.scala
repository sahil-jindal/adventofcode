package day01

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

type Pair = (first: List[Int], second: List[Int])

def parseNumbers(line: String) = raw"(\d+)".r.findAllIn(line).map(_.toInt).toList

def parseInput(input: List[String]): Pair = {
    val List(num1, num2) = input.map(parseNumbers).transpose.map(_.sorted)
    return (num1, num2)
}

def evaluatorOne(input: Pair): Int = {
    val (first, second) = input
    return (first zip second).map { case (a, b) => (a - b).abs }.sum
}

def evaluatorTwo(input: Pair): Int = {
    val (first, second) = input
    val weights = second.groupMapReduce(identity)(_ => 1)(_ + _)

    return first.withFilter(weights.contains).map(num => weights(num) * num).sum
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