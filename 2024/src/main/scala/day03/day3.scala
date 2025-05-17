package day03

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

val firstRegex = raw"mul\((\d+),(\d+)\)".r
val secondRegex = raw"mul\((\d+),(\d+)\)|don't\(\)|do\(\)".r

def evalutorOne(input: List[String]): Int = {
    var sum = 0

    for (line <- input) { 
        val numberString = firstRegex.findAllIn(line).toArray
        
        for (str <- numberString) {
            val List(first, second) = firstRegex.findFirstMatchIn(str).get.subgroups
            sum += (first.toInt * second.toInt)
        }
    }

    return sum
}

def evalutorTwo(input: List[String]): Int = {
    var sum = 0
    var isEnabled = true

    for (line <- input) { 
        val numberString = secondRegex.findAllIn(line).toArray
        
        for (str <- numberString) {
            str match {
                case "don't()" => isEnabled = false
                case "do()" => isEnabled = true
                case secondRegex(first, second) => {
                    if isEnabled then sum += (first.toInt * second.toInt)
                }
                case _ => "Not Found!!"
            }
        } 
    }

    return sum
}

def readinputFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readinputFromFile("day03.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evalutorOne(lines)}")        
            println(s"Part Two: ${evalutorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}