package day02

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

enum Sign { case Rock, Paper, Scissors }

case class Pair(elf: Char, human: Char)

def parseInput(input: List[String]): List[Pair] = input.map(it => Pair(it(0), it(2)))

def next(sign: Sign): Sign = sign match {
    case Sign.Rock => Sign.Paper 
    case Sign.Paper => Sign.Scissors
    case Sign.Scissors => Sign.Rock
}

def score(elfSign: Sign, humanSign: Sign): Int = {
    if (humanSign == next(elfSign)) return 6 + humanSign.ordinal + 1 
    if (humanSign == elfSign) return 3 + humanSign.ordinal + 1 
    if (humanSign == next(next(elfSign))) return 0 + humanSign.ordinal + 1 
    
    throw Exception()
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

def humanTwo(move: Pair): Sign = move match {   
    case Pair(first, 'X') => next(next(elf(first))) // elf wins
    case Pair(first, 'Y') => elf(first)             // draw
    case Pair(first, 'Z') => next(elf(first))       // you win
    case _ => throw Exception()
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