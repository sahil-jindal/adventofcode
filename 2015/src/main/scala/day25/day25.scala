package day25

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def evaluatorOne(rowDst: Int, colDst: Int) = {
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
    
    m
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("day25.txt") match
        case Success(lines) => {
            val dimensions = raw"(\d+)".r.findAllIn(lines.head).toArray.map(_.toInt)
            println(s"Part One: ${evaluatorOne(dimensions(0), dimensions(1))}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }