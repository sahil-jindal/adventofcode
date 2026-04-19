package day25

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def snafuToLong(snafu: String): Long = {
    val snafuNumbers = snafu.collect {
        case '=' => -2
        case '-' => -1
        case '0' => 0
        case '1' => 1
        case '2' => 2
    }

    return snafuNumbers.foldLeft(0L) { case (acc, item) => acc * 5 + item }
}

// Convert to decimal by first finding the result modulus 5 for each digit.
// If the answer is 3 or 4 then we must add a carry to the next digit to account for the
// subtraction.

def longToSnafu(num: Long): String = {
    // If the remainder of n is 3 or higher then this will add a carry digit to account
    // for the subtraction.
    val update: Long => Long = n => (n + 2) / 5

    val mapChar: PartialFunction[Long, Char] = { 
        case 0 => '0'
        case 1 => '1'
        case 2 => '2'
        case 3 => '='
        case 4 => '-'
    }
    
    return LazyList.iterate(num)(update)
        .takeWhile(_ > 0).map(_ % 5)
        .collect(mapChar).reverse.mkString
}

def solver(input: List[String]): String = longToSnafu(input.map(snafuToLong).sum)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day25.txt") match {
        case Success(lines) => println(s"Answer: ${solver(lines)}")
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}