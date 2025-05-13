package day09

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def expand(input: String, start: Int, lim: Int, recursive: Boolean): Long = {
    var res = 0L
    var i = start

    while (i < lim) {
        if input(i) == '(' then {
            val j = input.indexOf(')', i + 1)
            val m = raw"(\d+)x(\d+)".r.findFirstMatchIn(input.substring(i + 1, j)).get
            val length = m.group(1).toInt
            val mul = m.group(2).toInt
            val totalLength = 
                if recursive then expand(input, j + 1, j + length + 1, recursive) * mul.toLong 
                else (length * mul).toLong 
            res += totalLength
            i = j + length + 1
        } else {
            res += 1
            i += 1
        } 
    }

    return res
}

def evaluatorOne(input: String): Long = expand(input, 0, input.length, false)
def evaluatorTwo(input: String): Long = expand(input, 0, input.length, true)

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