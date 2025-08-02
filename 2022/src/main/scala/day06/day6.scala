package day06

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def startOfBlock(input: String, len: Int): Int = {
    return len + input.sliding(len).map(_.toSet.size).indexOf(len)
}

def evaluatorOne(input: String): Int = startOfBlock(input, 4)
def evaluatorTwo(input: String): Int = startOfBlock(input, 14)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day06.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines.head)}")
            println(s"Part Two: ${evaluatorTwo(lines.head)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}