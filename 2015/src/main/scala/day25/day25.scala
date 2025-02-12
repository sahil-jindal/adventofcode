package day25

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(line: String): Vector[Int] = raw"(\d+)".r.findAllIn(line).map(_.toInt).toVector

def evaluatorOne(rowDst: Int, colDst: Int): Long = {
    var m = 20151125L;
    var (irow, icol) = (1, 1);
    
    while (irow != rowDst || icol != colDst) {
        irow -= 1
        icol += 1
            
        if (irow == 0) {
            irow = icol;
            icol = 1;
        }
        
        m = (m * 252533L) % 33554393L;
    }
    
    return m
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day25.txt") match {
        case Success(lines) => {
            val Vector(rowDst, colDst) = parseInput(lines.head)
            println(s"Part One: ${evaluatorOne(rowDst, colDst)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}