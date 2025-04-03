package day25

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def solve(input: List[String]): Long = {
    val numbers = input.map(_.toLong)

    val mod = 20201227L
    var pow = 0
    var subj = 7L
    var num = subj
    
    while (num != numbers(0) && num != numbers(1)) {
        num = (num * subj) % mod
        pow += 1
    }

    subj = if (num == numbers(0)) numbers(1) else numbers(0)
    num = subj
    
    while (pow > 0) {
        num = (num * subj) % mod
        pow -= 1
    }

    return num
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day25.txt") match {
        case Success(lines) => println(s"Part One: ${solve(lines)}")
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}