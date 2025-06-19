package day02

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

enum Sign { case Rock, Paper, Scissors }

case class Pair(elf: Char, human: Char)

def parseInput(input: List[String]): List[Pair] = input.map(it => Pair(it(0), it(2)))

def score(elfSign: Sign, humanSign: Sign): Int = {
    val p1 = elfSign.ordinal
    val p2 = humanSign.ordinal

    if ((p1 + 1) % 3 == p2) return 6 + p2 + 1 
    if ((p1 + 2) % 3 == p2) return 0 + p2 + 1 
    
    return 3 + p2 + 1 
}

def elf(move: Char): Sign = move match {
    case 'A' => Sign.Rock
    case 'B' => Sign.Paper
    case 'C' => Sign.Scissors
    case _ => throw Exception() 
}

def total(input: List[Pair], human: Pair => Sign): Int = {
    return input.map(it => score(elf(it.elf), human(it))).sum
}

def humanOne(move: Pair): Sign = move.human match {   
    case 'X' => Sign.Rock 
    case 'Y' => Sign.Paper 
    case 'Z' => Sign.Scissors 
    case _ => throw Exception()
}

def humanTwo(move: Pair): Sign = {
    val p1 = elf(move.elf).ordinal

    move.human match {   
        case 'X' => Sign.fromOrdinal((p1 + 2) % 3) // elf wins
        case 'Y' => Sign.fromOrdinal(p1)           // draw
        case 'Z' => Sign.fromOrdinal((p1 + 1) % 3) // you win
        case _ => throw Exception()
    }
}

def evaluatorOne(input: List[Pair]) = total(input, humanOne)
def evaluatorTwo(input: List[Pair]) = total(input, humanTwo)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day02.txt") match {
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