package day15

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def generator(start: Long, mul: Long): Iterator[Long] = {
    Iterator.iterate(start)(state => (state * mul) % 2147483647)
}

def parseGenerators(input: List[String]): (Iterator[Long], Iterator[Long]) = {
    val startA = input(0).stripPrefix("Generator A starts with ").toLong
    val startB = input(1).stripPrefix("Generator B starts with ").toLong
    (generator(startA, 16807), generator(startB, 48271))
}

def countMatches(genA: Iterator[Long], genB: Iterator[Long], iterations: Int): Int = {
    genA.zip(genB).take(iterations).count { case (a, b) => (a & 0xFFFF) == (b & 0xFFFF) }
}

def evaluatorOne(input: List[String]): Long = {
    val (genA, genB) = parseGenerators(input)
    countMatches(genA, genB, 40000000)
}

def evaluatorTwo(input: List[String]): Long = {
    val (genA, genB) = parseGenerators(input)
    val filteredA = genA.filter(_ % 4 == 0)
    val filteredB = genB.filter(_ % 8 == 0)
    countMatches(filteredA, filteredB, 5000000)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

@main
def hello(): Unit =
    readLinesFromFile("day15.txt") match
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }