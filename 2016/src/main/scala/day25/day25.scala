package day25

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: List[String]): Int = {
    val first = raw"cpy (-?\d+) c".r.findFirstMatchIn(input(1)).get.group(1).toInt
    val second = raw"cpy (-?\d+) b".r.findFirstMatchIn(input(2)).get.group(1).toInt
    return first * second
}

def solver(offset: Int): Int = {
    return Iterator.iterate(0)(it => (it << 2) | 2)
        .dropWhile(_ < offset).next() - offset
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day25.txt") match {
        case Success(lines) => println(s"Answer: ${solver(parseInput(lines))}")
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}