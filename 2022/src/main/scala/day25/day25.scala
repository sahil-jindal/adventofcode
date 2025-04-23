package day25

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def snafuToLong(snafu: String): Long = {
    val snafuNumbers = snafu.map(digit => {
        digit match {
            case '=' => -2L
            case '-' => -1L
            case it => it.asDigit.toLong      
        } 
    })

    snafuNumbers.reduce { case (acc, item) => acc * 5L + item }
}

// Snafu numbers have digits -2, -1, 0, 1 and 2, so this is almost 
// standard base 5 conversion, but when dealing with digits 3 and 4 we 
// need to increment the higher decimal place so that we have
// something to subtract 2 and 1 from.
def longToSnafu(num: Long): String = {
    var d = num
    var res = new StringBuffer

    while (d > 0) {
        (d % 5) match {
            case 0 => res.insert(0, '0')
            case 1 => res.insert(0, '1')
            case 2 => res.insert(0, '2')
            // add 5 and emit -2 because 3 = 5 -2
            case 3 => d += 5; res.insert(0, '=') 
            // add 5 and emit -1 because 4 = 5 -1
            case 4 => d += 5; res.insert(0, '-')
        }

        d /= 5
    }
    
    return res.toString()
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