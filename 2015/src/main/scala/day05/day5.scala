package day05

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

val pairRule = "(..).*\\1".r
val repeatRule = "(.).\\1".r

def vowels = Set('a', 'e', 'i', 'o', 'u')
def disAllowedStrings = Set("ab", "cd", "pq", "xy")

def checksCondition(line: String): Boolean = {
    if line.filter(vowels.contains).size < 3 then return false
    if line.sliding(2).exists(disAllowedStrings.contains) then return false
    return (line.init zip line.tail).exists { case (a, b) => a == b }
}

def evaluatorOne(lines: List[String]): Int = lines.count(checksCondition)

def evaluatorTwo(lines: List[String]): Int = lines.count(line => {
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