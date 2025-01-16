package daythree

import scala.util.{Try, Success, Failure}
import scala.io.Source
import scala.collection.mutable.Set

class Direction(val dx: Int, val dy: Int)
case class Point(val x: Int, val y: Int)

def getDirections(arrow: Char) = arrow match {
    case '^' => Direction(0, 1)
    case '>' => Direction(1, 0)
    case 'v' => Direction(0, -1)
    case '<' => Direction(-1, 0)
    case _ => Direction(0, 0) 
}

def evaluatorOne(line: String) = {
    var currentPoint = Point(0, 0)
    val allPoints = Set[Point](currentPoint)

    line.foreach(arrow => {
        val currentDirection = getDirections(arrow)
        
        currentPoint = Point(
            currentPoint.x + currentDirection.dx,
            currentPoint.y + currentDirection.dy
        )

        allPoints.add(currentPoint)
    })

    allPoints
}

def evaluatorTwo(line: String) = {
    val (evenIndexedChars, oddIndexedChars) = line.zipWithIndex.partition { 
        case (_, index) => index % 2 == 0 
    }

    val santaDirections = evenIndexedChars.map(_._1).mkString
    val roboSantaDirections = oddIndexedChars.map(_._1).mkString

    val p1 = evaluatorOne(santaDirections)
    val p2 = evaluatorOne(roboSantaDirections)

    p1 | p2
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Try {
        val source = Source.fromResource(filePath)

        try {
            source.getLines().toList
        } finally {
            source.close()
        }
    }

@main
def hello(): Unit =
    readLinesFromFile("daythree.txt") match
        case Success(lines) => {
            val line = lines(0)
            println(s"${evaluatorTwo(line).size}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }