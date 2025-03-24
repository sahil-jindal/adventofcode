package day01

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: List[String]): Set[Int] = input.map(_.toInt).toSet

def evaluatorOne(numbers: Set[Int]): Int = {
    return (for {
        x <- numbers;
        y = 2020 - x
        if numbers.contains(y)
    } yield x * y).head
}

def evaluatorTwo(numbers: Set[Int]): Int = {
    return (for {
        x <- numbers;
        y <- numbers;
        z = 2020 - x - y
        if numbers.contains(z)
    } yield x * y * z).head
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