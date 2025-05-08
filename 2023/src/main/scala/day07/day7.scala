package day07

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Pair(hand: String, bid: Int)

def parseInput(input: List[String]) = input.map(line => {
    val parts = line.split(" ")
    Pair(parts(0), parts(1).toInt)
})

def solve(input: List[Pair], getPoints: String => (Long, Long)): Int = {
    val bidByRanking = input.sortBy(it => getPoints(it.hand)).map(_.bid)
    return bidByRanking.zipWithIndex.map { case (bid, rank) => (rank + 1) * bid }.sum
}

// Encode a list of numbers into a base 16 number
def encode(numbers: Seq[Int]): Long = numbers.foldLeft(0L) { case (acc, v) => acc * 16 + v }

// map cards to the number of their occurrences in the hand then order them 
// such thatA8A8A becomes 33322, 9A34Q becomes 11111 and K99AA becomes 22221
def patternValue(hand: String): Long = encode(hand.map(card => hand.count(_ == card)).sorted(using Ordering.Int.reverse))

// map cards to their indices in cardOrder. E.g. for 123456789TJQKA
// A8A8A becomes (13)(7)(13)(7)(13), 9A34Q becomes (8)(13)(2)(3)(11)
def cardValue(hand: String, cardOrder: String): Long = encode(hand.map(card => cardOrder.indexOf(card)))

def partOnePoints(hand: String): (Long, Long) = (patternValue(hand), cardValue(hand, "123456789TJQKA"))

def partTwoPoints(hand: String): (Long, Long) = {
    val cardOrder = "J123456789TQKA"
    val pattern = cardOrder.map(ch => patternValue(hand.replace('J', ch))).max
    return (pattern, cardValue(hand, cardOrder))
}

def evaluatorOne(input: List[Pair]): Int = solve(input, partOnePoints)
def evaluatorTwo(input: List[Pair]): Int = solve(input, partTwoPoints)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day07.txt") match {
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