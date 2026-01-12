package day07

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Pair(hand: String, bid: Int)

def parseInput(input: List[String]) = input.collect {
    case s"$a $b" => Pair(a, b.toInt)
}

def solve(input: List[Pair], getPoints: String => (Int, Int)): Int = {
    val bidByRanking = input.sortBy(it => getPoints(it.hand)).map(_.bid)
    return bidByRanking.zipWithIndex.map { case (bid, rank) => (rank + 1) * bid }.sum
}

// Encode a list of numbers into a base 16 number
def encode(digits: IndexedSeq[Int]): Int = {
    require(digits.forall((0 until 16).contains))

    val digitChars = digits.map {
        case d if d < 10 => ('0' + d).toChar
        case d => ('a' + (d - 10)).toChar
    }.mkString

    return Integer.parseInt(digitChars, 16)
}

// map cards to the number of their occurrences in the hand then order them 
// such that A8A8A becomes 33322, 9A34Q becomes 11111 and K99AA becomes 22221
def patternValue(hand: String): Int = {
    val freq = hand.groupMapReduce(identity)(_ => 1)(_ + _)
    return encode(hand.map(freq).sorted(using Ordering.Int.reverse))   
}

// map cards to their indices in cardOrder. E.g. for 123456789TJQKA
// A8A8A becomes (13)(7)(13)(7)(13), 9A34Q becomes (8)(13)(2)(3)(11)
def cardValue(hand: String, cardOrder: String): Int = encode(hand.map(card => cardOrder.indexOf(card)))

def partOnePoints(hand: String): (Int, Int) = (patternValue(hand), cardValue(hand, "123456789TJQKA"))

def partTwoPoints(hand: String): (Int, Int) = {
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