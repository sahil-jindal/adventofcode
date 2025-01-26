package dayninteen

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.util.Random

class ReplacementString(
    val from: Int,
    val length: Int,
    val to: String
)

def parseInput(lines: List[String]) = {
    val index = lines.indexWhere(_.trim.isEmpty)
    val (from, to) = lines.splitAt(index)
    val rules = from.map { case s"$a => $b" => (a, b) }.toArray

    (rules, to(1))
}

def Replacements(molecule: String, rules: Array[(String, String)]) = {
    val groupedRules = rules.groupBy { case (from, _) => from.length }
    val replacements = ListBuffer[ReplacementString]()

    for (len, similarRules) <- groupedRules do {
        val possibleStrings = molecule.sliding(len).toList

        for i <- 0 until possibleStrings.size do {
            for (from, to) <- similarRules do {
                if from == possibleStrings(i) then {
                    replacements += ReplacementString(i, possibleStrings(i).length, to)
                }
            }
        }
    }

    replacements.toList
}

def Replace(molecule: String, from: Int, length: Int, to: String) = {
    molecule.substring(0, from) + to + molecule.substring(from + length)
}

def evaluatorOne(molecule: String, rules: Array[(String, String)]) = {
    Replacements(molecule, rules).map { it => Replace(molecule, it.from, it.length, it.to) }.toSet.size
}

def evaluatorTwo(molecule: String, rules: Array[(String, String)]) = {
    val reversedRules = rules.map { case (from, to) => (to, from) }
    val random = Random()
    var current = molecule
    var depth = 0

    while (current != "e") {
        val replacements = Replacements(current, reversedRules)
        
        if (replacements.isEmpty) {
            current = molecule
            depth = 0
        } else {
            val replacement = replacements(random.nextInt(replacements.length))
            current = Replace(current, replacement.from, replacement.length, replacement.to)
            depth += 1
        }
    }

    depth
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("dayninteen.txt") match
        case Success(lines) => {
            val (rules, str) = parseInput(lines)
            println(evaluatorTwo(str, rules))
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }