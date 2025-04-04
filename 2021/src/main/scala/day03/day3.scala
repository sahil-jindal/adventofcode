package day03

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.control.Breaks._

def extractOne(input: List[String], selectBitAt: (List[String], Int) => Char): Int = {
    val cbit = input.head.length()
    val bits = (0 until cbit).map(ibit => selectBitAt(input, ibit)).mkString("")
    return Integer.parseInt(bits, 2)
}

def extractTwo(input: List[String], selectBitAt: (List[String], Int) => Char): Int = {
    var lines = input
    val cbit = input.head.length()

    breakable {
        for (ibit <- 0 until cbit) {
            val bit = selectBitAt(lines, ibit)
            lines = lines.filter(line => line(ibit) == bit)
            if (lines.size <= 1) break()
        }
    }

    return Integer.parseInt(lines.head, 2)
}

def mostCommonBitAt(lines: List[String], ibit: Int): Char = {
    return if (2 * lines.count(line => line(ibit) == '1') >= lines.size) then '1' else '0' 
}

def leastCommonBitAt(lines: List[String], ibit: Int): Char = {
    return if (mostCommonBitAt(lines, ibit) == '1') then '0' else '1'
}

def gammaRate(diagnosticReport: List[String]): Int = extractOne(diagnosticReport, mostCommonBitAt)
def epsilonRate(diagnosticReport: List[String]): Int = extractOne(diagnosticReport, leastCommonBitAt)
def co2ScruberRating(diagnosticReport: List[String]): Int = extractTwo(diagnosticReport, leastCommonBitAt)
def oxygenGeneratorRating(diagnosticReport: List[String]): Int = extractTwo(diagnosticReport, mostCommonBitAt)

def evaluatorOne(input: List[String]): Int = gammaRate(input) * epsilonRate(input)
def evaluatorTwo(input: List[String]): Int = co2ScruberRating(input) * oxygenGeneratorRating(input)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day03.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}