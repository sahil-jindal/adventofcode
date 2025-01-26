package day05

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def vowels = Set('a', 'e', 'i', 'o', 'u')
def disAllowedStrings = Set("ab", "cd", "pq", "xy")

def vowelsCondition(line: String): Boolean = {
    line.filter(vowels.contains).size >= 3
}

def repeatedLetterCondition(line: String): Boolean = {
    line.sliding(2).exists(it => it.charAt(0) == it.charAt(1))
}

def disAllowedStringsCondition(line: String): Boolean = {
    !line.sliding(2).exists(it => disAllowedStrings.contains(it))
}

def evaluatorOne(line: String): Boolean = {
    vowelsCondition(line) && repeatedLetterCondition(line) && disAllowedStringsCondition(line)
}

def evaluatorTwo(string: String): Boolean = {
    val pairRule = "(..).*\\1".r
    val repeatRule = "(.).\\1".r

    pairRule.findFirstIn(string).isDefined && repeatRule.findFirstIn(string).isDefined
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("day05.txt") match
        case Success(lines) => {
            println(s"Part One: ${lines.count(evaluatorOne)}")
            println(s"Part Two: ${lines.count(evaluatorTwo)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }