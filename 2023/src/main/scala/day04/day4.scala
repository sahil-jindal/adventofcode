package day04

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Card(matches: Int)

def parseInput(input: List[String]) = input.map(line => {
    val parts = line.split(Array(':', '|'))
    val l = raw"(\d+)".r.findAllIn(parts(1)).toSet
    val r = raw"(\d+)".r.findAllIn(parts(2)).toSet
    Card((l & r).size)
})

def evaluatorOne(cards: List[Card]): Int = {
    return cards.collect { case Card(matches) if matches > 0 => 1 << (matches - 1) }.sum
}

def evaluatorTwo(cards: List[Card]): Int = {
    val counts = Array.fill(cards.length)(1)

    for ((card, i) <- cards.zipWithIndex; j <- 0 until card.matches) {
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