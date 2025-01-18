package dayone

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def floorMovement(ch: Char) = ch match
    case '(' => 1
    case ')' => -1
    case _ => 0

def evaluatorOne(line: String) = line.map(floorMovement).sum

def evaluatorTwo(line: String) = {
    val floors = Array.ofDim[Int](line.length + 1)

    floors(0) = 0

    for i <- 1 to line.length do {
        floors(i) = floorMovement(line.charAt(i - 1)) + floors(i - 1) 
    }

    floors.indexOf(-1)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("dayone.txt") match
        case Success(lines) => {
            val line = lines(0)
            println(s"${evaluatorTwo(line)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }