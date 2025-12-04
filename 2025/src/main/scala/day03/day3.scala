package day03

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Queue

def maxSubsequence(num: String, k: Int): Long = {
    var toDrop = num.length - k
    val stack = new Queue[Char](k)

    for (c <- num) {
        while (stack.nonEmpty && toDrop > 0 && stack.last < c) {
            stack.removeLast()
            toDrop -= 1
        }

        stack.append(c)
    }

    return stack.take(k).mkString.toLong
}

def selections(numbers: List[String], n: Int) = numbers.map(it => maxSubsequence(it, n)).sum

def evaluatorOne(numbers: List[String]) = selections(numbers, 2)
def evaluatorTwo(numbers: List[String]) = selections(numbers, 12)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day03.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}