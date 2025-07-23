package day19

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

sealed trait Rule
case class Literal(char: Char) extends Rule
case class Alternative(options: List[List[Int]]) extends Rule

case class Pair(rules: Map[Int, Rule], messages: List[String])

def parseRules(line: String) = {
    val Array(idStr, ruleStr) = line.split(": ")
    
    val rule = if (ruleStr.contains("\"")) {
        Literal(ruleStr.charAt(1))
    } else {
        Alternative(ruleStr.split(" \\| ").map(_.trim.split(" ").map(_.toInt).toList).toList)
    }
    
    idStr.toInt -> rule
}

def parseInput(input: List[String]): Pair = {
    val idx = input.indexWhere(_.trim.isEmpty)
    return Pair(input.take(idx).map(parseRules).toMap, input.drop(idx + 1))
}

def buildRegex(rules: Map[Int, Rule], id: Int, partTwo: Boolean): String = {
    if (partTwo) {
        if (id == 8) {
            // rule 8: 42 | 42 8  => 42+
            return s"(${buildRegex(rules, 42, true)})+"
        } 
        
        if (id == 11) {
            // rule 11: 42 31 | 42 11 31
            // This creates a rule that matches 42{n}31{n} for n from 1 to 5
            // We can't express this with regular expressions perfectly, but we can approximate
            val r42 = buildRegex(rules, 42, true)
            val r31 = buildRegex(rules, 31, true)
        
            return (1 to 5).map(n => s"(${r42}){$n}(${r31}){$n}").mkString("(", "|", ")")
        }
    }
    
    return rules(id) match {
        case Literal(char) => char.toString
        case Alternative(options) => options.map(_.map(buildRegex(rules, _, partTwo)).mkString).mkString("(", "|", ")")
    }
}

def countValidMessages(input: Pair, partTwo: Boolean): Int = {
    val Pair(rules, messages) = input
    val regex = raw"^${buildRegex(rules, 0, partTwo)}$$".r
    return messages.count(msg => regex.pattern.matcher(msg).matches())
}

def evaluatorOne(input: Pair): Int = countValidMessages(input, false)
def evaluatorTwo(input: Pair): Int = countValidMessages(input, true)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day19.txt") match {
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