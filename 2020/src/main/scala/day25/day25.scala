package day25

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def solver(input: List[String]): Long = {
    val List(doorKey, cardKey) = input.map(_.toLong)

    val mod = 20201227L
    var pow = 0
    var subj = 7L
    var num = subj
    
    while (num != doorKey && num != cardKey) {
        num = (num * subj) % mod
        pow += 1
    }

    subj = if (num == doorKey) cardKey else doorKey
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
        case Success(lines) => println(s"Answer: ${solver(lines)}")
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}