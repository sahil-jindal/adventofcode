package day02

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

enum Sign { case Rock, Paper, Scissors }
enum Human { case X, Y, Z }

case class Pair(elf: Sign, human: Human)

def parseElf: PartialFunction[Char, Sign] = {
    case 'A' => Sign.Rock
    case 'B' => Sign.Paper
    case 'C' => Sign.Scissors 
}

def parseHuman: PartialFunction[Char, Human] = {
    case 'X' => Human.X
    case 'Y' => Human.Y
    case 'Z' => Human.Z
}

def parseInput(input: List[String]) = input.map(it => {
    Pair(parseElf(it(0)), parseHuman(it(2)))
})

def score(elfSign: Sign, humanSign: Sign): Int = {
    val p1 = elfSign.ordinal
    val p2 = humanSign.ordinal

    if ((p1 + 1) % 3 == p2) return 6 + p2 + 1 
    if ((p1 + 2) % 3 == p2) return 0 + p2 + 1 
    
    return 3 + p2 + 1 
}

def total(input: List[Pair], human: Pair => Sign): Int = {
    return input.map(it => score(it.elf, human(it))).sum
}

def humanOne(move: Pair): Sign = move.human match {   
    case Human.X => Sign.Rock 
    case Human.Y => Sign.Paper 
    case Human.Z => Sign.Scissors
}

def humanTwo(move: Pair): Sign = {
    val p1 = move.elf.ordinal

    move.human match {   
        case Human.X => Sign.fromOrdinal((p1 + 2) % 3) // elf wins
        case Human.Y => Sign.fromOrdinal(p1)           // draw
        case Human.Z => Sign.fromOrdinal((p1 + 1) % 3) // you win
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