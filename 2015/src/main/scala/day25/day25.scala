package day25

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def solver(input: String): Long = {
    val Seq(rowDst, colDst) = raw"(\d+)".r.findAllIn(input).map(_.toInt).toSeq

    val d = rowDst + colDst - 1
    val k = (d * (d - 1)) / 2 + colDst // Anti-diagonal encoding

    var m = 20151125L

    for (_ <- 1 until k) {
        m = (m * 252533L) % 33554393L
    }
    
    return m
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day25.txt") match {
        case Success(lines) => println(s"Answer: ${solver(lines.head)}")
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}