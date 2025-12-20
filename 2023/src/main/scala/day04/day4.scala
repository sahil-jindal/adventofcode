package day04

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: List[String]) = input.map(line => {
    val Array(_, b, c) = line.split(Array(':', '|'))
    val l = raw"(\d+)".r.findAllIn(b).toSet
    val r = raw"(\d+)".r.findAllIn(c).toSet
    (l & r).size
})

def evaluatorOne(cards: List[Int]): Int = {
    return cards.withFilter(_ > 0).map(it => 1 << (it - 1)).sum
}

def evaluatorTwo(cards: List[Int]): Int = {
    val counts = Array.fill(cards.length)(1)

    for ((matches, i) <- cards.zipWithIndex; j <- 0 until matches) {
        counts(i + j + 1) += counts(i)
    }

    return counts.sum
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day04.txt") match {
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