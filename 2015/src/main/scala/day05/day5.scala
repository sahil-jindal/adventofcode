package day05

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

val pairRule = raw"(..).*\1".r
val repeatRule = raw"(.).\1".r

def vowels = Set('a', 'e', 'i', 'o', 'u')
def disAllowedStrings = Set("ab", "cd", "pq", "xy")

def checksCondition(input: String): Boolean = {
    if (input.count(vowels.contains) < 3) return false
    if (input.sliding(2).exists(disAllowedStrings.contains)) return false
    return (input.init zip input.tail).exists { case (a, b) => a == b }
}

def evaluatorOne(input: List[String]): Int = input.count(checksCondition)

def evaluatorTwo(input: List[String]): Int = input.count(line => {
    pairRule.findFirstIn(line).isDefined && repeatRule.findFirstIn(line).isDefined
})

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day05.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}