package day25

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def solver(input: String): Int = {
    val Seq(rowDst, colDst) = raw"(\d+)".r.findAllIn(input).map(_.toInt).toSeq

    val d = rowDst + colDst - 1
    val k = (d * (d - 1)) / 2 + colDst // Anti-diagonal encoding

    val m = BigInt(33554393)
    
    return (BigInt(20151125) * BigInt(252533).modPow(k - 1, m)).mod(m).intValue()
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day25.txt") match {
        case Success(lines) => println(s"Answer: ${solver(lines.head)}")
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}