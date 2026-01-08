package day23

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

// # Safe Cracking
//
// Like [`Day 12`] this problem is all about *reading* code not writing code.
//
// We could implement a brute force virtual machine without understanding the underlying code
// but it's much more efficient to analyze the code instead.
//
// The first thing we notice is that the following idiom is repeated several times:
//
// ```none
//     inc x
//     dec y
//     jnz y -2
// ```
//
// This is equivalent to `x += y` only much less efficient. The `tgl` instruction eventually
// rewrites a `jnz` to `cpy` to allow the program loop to end.
//
// Analysis shows that the code is calculating the [factorial](https://en.wikipedia.org/wiki/Factorial)
// of `a` plus some constant offset. We can replace the entire code with a single multiplication.
// If we had emulated the raw instructions directly then it would have taken billions of
// iterations to get the answer.

def parseInput(input: List[String]): Int = {
    val first = raw"(-?\d+)".r.findFirstMatchIn(input(19)).get.group(1).toInt
    val second = raw"(-?\d+)".r.findFirstMatchIn(input(20)).get.group(1).toInt
    return first * second
}

def evaluatorOne(input: Int): Int = 5040 + input
def evaluatorTwo(input: Int): Int = 479001600 + input

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day23.txt") match {
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