package day19

import scala.util.{Try, Success, Failure, Using, Random}
import scala.io.Source

case class ReplacementString(from: Int, length: Int, to: String)
case class Fabrication(rules: List[(String, String)], molecule: String)

def parseInput(input: List[String]): Fabrication = {
    val rules = input.dropRight(2).map { case s"$a => $b" => (a, b) }
    return Fabrication(rules, input.last)
}

def Replacements(fab: Fabrication): List[ReplacementString] = {
    val groupedRules = fab.rules.groupBy { case (from, _) => from.length }

    return (for {
        (len, similarRules) <- groupedRules
        (str, i) <- fab.molecule.sliding(len).zipWithIndex
        (from, to) <- similarRules
        if from == str
    } yield ReplacementString(i, len, to)).toList
}

def Replace(molecule: String, replaceString: ReplacementString): String = {
    val ReplacementString(from, length, to) = replaceString
    return molecule.substring(0, from) + to + molecule.substring(from + length)
}

def evaluatorOne(fab: Fabrication): Int = {
    return Replacements(fab).map(Replace(fab.molecule, _)).toSet.size
}

def evaluatorTwo(fab: Fabrication): Int = {
    val reversedRules = fab.rules.map { case (from, to) => (to, from) }
    val random = Random()
    var current = fab.molecule
    var depth = 0

    while (current != "e") {
        val replacements = Replacements(Fabrication(reversedRules, current))
        
        if (replacements.isEmpty) {
            current = fab.molecule
            depth = 0
        } else {
            val replacement = replacements(random.nextInt(replacements.length))
            current = Replace(current, replacement)
            depth += 1
        }
    }

    return depth
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day19.txt") match {
        case Success(lines) => {
            val fab = parseInput(lines)
            println(s"Part One: ${evaluatorOne(fab)}")
            println(s"Part Two: ${evaluatorTwo(fab)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}