package day19

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Fabrication(rules: List[(String, String)], molecule: String)

def parseInput(input: List[String]): Fabrication = {
    val rules = input.dropRight(2).collect { case s"$a => $b" => (a, b) }
    return Fabrication(rules, input.last)
}

def Replace(molecule: String, from: Int, length: Int, to: String): String = {
    return molecule.substring(0, from) + to + molecule.substring(from + length)
}

def evaluatorOne(fab: Fabrication): Int = {
    val Fabrication(rules, molecule) = fab

    return (for {
        (from, to) <- rules
        len = from.length
        i <- 0 to (molecule.length - len)
        if molecule.startsWith(from, i)
    } yield Replace(molecule, i, len, to)).toSet.size
}

def evaluatorTwo(fab: Fabrication): Int = {
    val molecule = fab.molecule
    
    val elements = molecule.count(_.isUpper)
    val rn = molecule.sliding(2).count(_ == "Rn")
    val ar = molecule.sliding(2).count(_ == "Ar")
    val y = molecule.count(_ == 'Y')

    return elements - rn - ar - 2*y - 1
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