package day15

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Generator(start: Long, generateNumber: Long => Long) {
    def numbers = Iterator.iterate(start)(generateNumber)
}

type Pair = (Generator, Generator)

def parseGenerator(line: String, mul: Long) = {
    val start = raw"Generator \w starts with (\d+)".r.findFirstMatchIn(line).get.group(1).toLong
    Generator(start, state => (state * mul) % 2147483647) 
}

def parseInput(input: List[String]): Pair = {
    return (parseGenerator(input(0), 16807), parseGenerator(input(1), 48271))
}

def countMatches(genA: Iterator[Long], genB: Iterator[Long], iterations: Int): Int = {
    return (genA zip genB).take(iterations).count { case (a, b) => (a & 0xFFFF) == (b & 0xFFFF) }
}

def evaluatorOne(input: Pair): Long = {
    val (genA, genB) = input
    return countMatches(genA.numbers, genB.numbers, 40000000)
}

def evaluatorTwo(input: Pair): Long = {
    val (genA, genB) = input
    val filteredA = genA.numbers.filter(_ % 4 == 0)
    val filteredB = genB.numbers.filter(_ % 8 == 0)
    return countMatches(filteredA, filteredB, 5000000)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day15.txt") match {
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