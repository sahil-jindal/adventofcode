package day25

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def evaluatorOne(input: String): Long = {
    val Seq(rowDst, colDst) = raw"(\d+)".r.findAllIn(input).map(_.toInt).toSeq

    var m = 20151125L
    var (y, x) = (1, 1)
    
    while (y != rowDst || x != colDst) {
        y -= 1
        x += 1
            
        if (y == 0) {
            y = x
            x = 1
        }
        
        m = (m * 252533L) % 33554393L
    }
    
    return m
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day25.txt") match {
        case Success(lines) => println(s"Part One: ${evaluatorOne(lines.head)}")
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}