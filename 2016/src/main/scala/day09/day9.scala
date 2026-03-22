package day09

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.control.Breaks._

def number(slice: String): (String, Long) = {
    val (digits, rest) = slice.span(_.isDigit)
    return (rest.tail, digits.toLong)
}

def decompress(input: String, partTwo: Boolean): Long = {
    var slice = input
    var length = 0L

    breakable {
        while (true) {
            val start = slice.indexOf('(')
            if (start == -1) break()

            val (temp1, amount) = number(slice.drop(start + 1))
            val (temp2, repeat) = number(temp1)
            val (first, second) = temp2.splitAt(amount.toInt)

            val result = if (partTwo) decompress(first, true) else amount

            slice = second
            length += start + result * repeat
        }
    }

    return length + slice.size
}

def evaluatorOne(input: String): Long = decompress(input, false)
def evaluatorTwo(input: String): Long = decompress(input, true)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day09.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines.head)}")
            println(s"Part Two: ${evaluatorTwo(lines.head)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}
