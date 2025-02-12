package day19

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.util.Random

class ReplacementString(val from: Int, val length: Int, val to: String)
class Fabrication(val rules: Array[(String, String)], val molecule: String)

def parseInput(lines: List[String]): Fabrication = {
    val index = lines.indexWhere(_.trim.isEmpty)
    val (from, to) = lines.splitAt(index)
    val rules = from.map { case s"$a => $b" => (a, b) }.toArray
    return Fabrication(rules, to(1))
}

def Replacements(fab: Fabrication): List[ReplacementString] = {
    val groupedRules = fab.rules.groupBy { case (from, _) => from.length }
    val replacements = ListBuffer[ReplacementString]()

    for (len, similarRules) <- groupedRules do {
        for (str, i) <- fab.molecule.sliding(len).zipWithIndex do {
            for (from, to) <- similarRules do {
                if from == str then replacements += ReplacementString(i, str.length, to)
            }
        }
    }

    return replacements.toList
}

def Replace(molecule: String, repstr: ReplacementString): String = {
    return molecule.substring(0, repstr.from) + repstr.to + molecule.substring(repstr.from + repstr.length)
}

def evaluatorOne(fab: Fabrication): Int = {
    return Replacements(fab).map { it => Replace(fab.molecule, it) }.toSet.size
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