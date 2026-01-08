package day12

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

// # Leonardo's Monorail
//
// This problem is interesting in that the solution is all about *reading* code not writing code.
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
// This is equivalent to `x += y` only much less efficient. Replacing this in the code then
// rewriting the remainder to Rust the program becomes:
//
// ```none
//     let mut a = 1;
//     let mut b = 1;
//     let mut c = 0; // 1 in part two
//     let d = if c == 0 { 26 } else { 33 };
//     for _ in 0..d {
//         c = a;
//         a += b;
//         b = c;
//     }
//     a += q * r // q and r are the constants on lines 17 and 18.
// ```
//
// We can see that the code is calculating the 28th and 35th numbers in the Fibonacci sequence
// plus some constant offset. We can replace the entire code with a single multiplication.
// If we had emulated the raw instructions then it would have taken ~10,000,000 iterations to
// obtain the answer.

def parseInput(input: List[String]): Int = {
    val first = raw"(-?\d+)".r.findFirstMatchIn(input(16)).get.group(1).toInt
    val second = raw"(-?\d+)".r.findFirstMatchIn(input(17)).get.group(1).toInt
    return first * second
}

def evaluatorOne(input: Int): Int = 317811 + input
def evaluatorTwo(input: Int): Int = 9227465 + input

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day12.txt") match {
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