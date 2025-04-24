package day01

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.matching.Regex

def parseMatch(s: String): Int = s match {
    case "one"   => 1
    case "two"   => 2
    case "three" => 3
    case "four"  => 4
    case "five"  => 5
    case "six"   => 6
    case "seven" => 7
    case "eight" => 8
    case "nine"  => 9
    case d       => d.toInt
}

def solve(input: List[String], pattern: Regex): Int = input.map(line => {
    val matches = pattern.findAllMatchIn(line).map(_.group(1)).toList
    parseMatch(matches.head) * 10 + parseMatch(matches.last)
}).sum

def evaluatorOne(input: List[String]): Int = solve(input, raw"(?=(\d))".r)
def evaluatorTwo(input: List[String]): Int = solve(input, raw"(?=(one|two|three|four|five|six|seven|eight|nine|\d))".r)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day01.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}