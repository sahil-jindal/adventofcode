package day02

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Direction(dy: Int, dx: Int)

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

val firstKeyPad = parseKeypad(List("123", "456", "789"))
val secondKeyPad = parseKeypad(List("  1  ", " 234 ", "56789", " ABC ", "  D  "))

def parseKeypad(input: List[String]): Map[Point, Char] = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
        if ch != ' '
    } yield Point(y, x) -> ch).toMap
}

def parseInput(input: List[String]) = input.map(_.collect {
    case 'U' => Direction(-1, 0)
    case 'R' => Direction(0, 1)
    case 'D' => Direction(1, 0)
    case 'L' => Direction(0, -1)
})

def getCode(paths: List[IndexedSeq[Direction]], keypad: Map[Point, Char]): String = {
    var temp = keypad.collectFirst { case (k, v) if v == '5' => k }.get
    val code = new StringBuilder

    for (path <- paths) {
        for (dir <- path) {
            val newP = temp + dir

            if (keypad.contains(newP)) {
                temp = newP
            } 
        }

        code.append(keypad(temp))
    }

    code.toString
}

def evaluatorOne(paths: List[IndexedSeq[Direction]]): String = getCode(paths, firstKeyPad)
def evaluatorTwo(paths: List[IndexedSeq[Direction]]): String = getCode(paths, secondKeyPad)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day02.txt") match {
        case Success(lines) => {
            val input = parseInput(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}