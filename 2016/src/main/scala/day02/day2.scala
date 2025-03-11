package day02

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Direction(val dy: Int, val dx: Int)
case class Point(val y: Int, val x: Int)

def firstKeyPad = List(
    List('1', '2', '3'),
    List('4', '5', '6'),
    List('7', '8', '9')
)

def secondKeyPad = List(
    List(' ', ' ', '1', ' ', ' '),
    List(' ', '2', '3', '4', ' '),
    List('5', '6', '7', '8', '9'),
    List(' ', 'A', 'B', 'C', ' '),
    List(' ', ' ', 'D', ' ', ' ')
)

def getDirections(dir: Char) = dir match {
    case 'U' => Direction(-1, 0)
    case 'R' => Direction(0, 1)
    case 'D' => Direction(1, 0)
    case 'L' => Direction(0, -1)
    case _ => Direction(0, 0)
}

def boundaryConditionOne(y: Int, x: Int): Boolean = (x >= 0 && x <= 2) && (y >= 0 && y <= 2)

def boundaryConditionTwo(y: Int, x: Int): Boolean = {
    return (x >= 0 && x <= 4) && (y >= 0 && y <= 4) && secondKeyPad(y)(x) != ' '
}

def getCode(
    currPoint: Point, directions: List[String], keyPad: List[List[Char]],
    boundaryCondition: (Int, Int) => Boolean
): String = {
    val code = new StringBuilder
    var temp = currPoint

    for line <- directions do {
        for dir <- line do {
            val move = getDirections(dir)

            val newY = temp.y + move.dy
            val newX = temp.x + move.dx

            if boundaryCondition(newY, newX) then {
                temp = Point(newY, newX)
            } 
        }

        code.append(keyPad(temp.y)(temp.x))
    }

    code.toString
}

def evaluatorOne(directions: List[String]): String = {
    return getCode(Point(1, 1), directions, firstKeyPad, boundaryConditionOne)
}

def evaluatorTwo(directions: List[String]): String = {
    return getCode(Point(2, 0), directions, secondKeyPad, boundaryConditionTwo)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day02.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}