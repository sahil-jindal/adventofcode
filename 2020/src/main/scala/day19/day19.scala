package day19

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

sealed trait Rule
case class Literal(char: Char) extends Rule
case class Alternative(options: Seq[Seq[Int]]) extends Rule

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def parseInput(input: List[String]): (Map[Int, Rule], List[String]) = {
    val parts = groupLines(input)

    val rules = parts(0).map(line => {
        val Array(idStr, ruleStr) = line.split(": ")
        val id = idStr.toInt
      
        val rule = if (ruleStr.contains("\"")) {
            Literal(ruleStr.charAt(1))
        } else {
            Alternative(ruleStr.split(" \\| ").map(_.trim.split(" ").map(_.toInt).toSeq).toSeq)
        }
      
        (id, rule)
    }).toMap
    
    return (rules, parts(1))
}

def buildRegex(rules: Map[Int, Rule], id: Int, partTwo: Boolean): String = {
    if (partTwo) {
        if (id == 8) {
            // rule 8: 42 | 42 8  => 42+
            return s"(${buildRegex(rules, 42, true)})+"
        } else if (id == 11) {
            // rule 11: 42 31 | 42 11 31
            // This creates a rule that matches 42{n}31{n} for n from 1 to 5
            // We can't express this with regular expressions perfectly, but we can approximate
            val r42 = buildRegex(rules, 42, true)
            val r31 = buildRegex(rules, 31, true)
        
            val options = (1 to 5).map(n => s"(${r42}){$n}(${r31}){$n}")
            return options.mkString("(", "|", ")")
        }
    }
    
    return rules(id) match {
        case Literal(char) => char.toString
        case Alternative(options) => options.map(_.map(buildRegex(rules, _, partTwo)).mkString).mkString("(", "|", ")")
    }
}

def countValidMessages(input: List[String], partTwo: Boolean): Int = {
    val (rules, messages) = parseInput(input)
    val regex = s"^${buildRegex(rules, 0, partTwo)}$$".r
    return messages.count(msg => regex.pattern.matcher(msg).matches())
}

def evaluatorOne(input: List[String]): Int = countValidMessages(input, false)
def evaluatorTwo(input: List[String]): Int = countValidMessages(input, true)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day19.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}