package day03

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.control.Breaks._

enum Bit { case Zero, One }

type Number = IndexedSeq[Bit]

def parseBits(ch: Char) = ch match {
    case '0' => Bit.Zero
    case '1' => Bit.One
    case _ => throw new Exception()
}

def parseInput(input: List[String]) = input.map(_.map(parseBits))

def calculateNumber(bits: Number) = bits.reverse.zipWithIndex.collect { case (Bit.One, i) => 1 << i }.sum

def extractOne(input: List[Number], selectBitAt: (List[Number], Int) => Bit): Int = {
    val bits = input.head.indices.map(ibit => selectBitAt(input, ibit))
    return calculateNumber(bits)
}

def extractTwo(input: List[Number], selectBitAt: (List[Number], Int) => Bit): Int = {
    var lines = input

    breakable {
        for (ibit <- lines.head.indices) {
            val bit = selectBitAt(lines, ibit)
            lines = lines.filter(line => line(ibit) == bit)
            if (lines.size <= 1) break()
        }
    }

    return calculateNumber(lines.head)
}

def mostCommonBitAt(lines: List[Number], ibit: Int): Bit = {
    return if (2 * lines.count(line => line(ibit) == Bit.One) >= lines.size) then Bit.One else Bit.Zero 
}

def leastCommonBitAt(lines: List[Number], ibit: Int): Bit = {
    return if (mostCommonBitAt(lines, ibit) == Bit.One) then Bit.Zero else Bit.One
}

def gammaRate(diagnosticReport: List[Number]): Int = extractOne(diagnosticReport, mostCommonBitAt)
def epsilonRate(diagnosticReport: List[Number]): Int = extractOne(diagnosticReport, leastCommonBitAt)
def co2ScruberRating(diagnosticReport: List[Number]): Int = extractTwo(diagnosticReport, leastCommonBitAt)
def oxygenGeneratorRating(diagnosticReport: List[Number]): Int = extractTwo(diagnosticReport, mostCommonBitAt)

def evaluatorOne(input: List[Number]): Int = gammaRate(input) * epsilonRate(input)
def evaluatorTwo(input: List[Number]): Int = co2ScruberRating(input) * oxygenGeneratorRating(input)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day03.txt") match {
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