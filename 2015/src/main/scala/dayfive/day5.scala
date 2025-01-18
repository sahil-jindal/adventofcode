package dayfive

import scala.util.{Try, Success, Failure}
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

 def isNicePart2(string: String): Boolean = {
    val pairRule = "(..).*\\1".r
    val repeatRule = "(.).\\1".r

    pairRule.findFirstIn(string).isDefined && repeatRule.findFirstIn(string).isDefined
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Try {
        val source = Source.fromResource(filePath)

        try {
            source.getLines().toList
        } finally {
            source.close()
        }
    }

def hello(): Unit =
    readLinesFromFile("dayfive.txt") match
        case Success(lines) => {
            val total = lines.count(isNicePart2)
            println(s"$total")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }