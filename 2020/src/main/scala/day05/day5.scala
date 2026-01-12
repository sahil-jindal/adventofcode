package day05

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: List[String]) = input.map(line => {
    val binaryString = line.collect {
        case 'B' | 'R' => '1'
        case 'F' | 'L' => '0'
    }
    
    Integer.parseInt(binaryString, 2)
}).toSet

def evaluatorOne(seats: Set[Int]): Int = seats.max

def evaluatorTwo(seats: Set[Int]): Int = {
    val (min, max) = (seats.min, seats.max)
    return (min to max).find(id => !seats.contains(id)).get
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day05.txt") match {
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