package day19

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Direction(dy: Int, dx: Int) {
    def rotateLeft = Direction(-dx, dy)
    def rotateRight = Direction(dx, -dy)
}

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

def parseInput(input: List[String]): Map[Point, Char] = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
        if !ch.isWhitespace
    } yield Point(y, x) -> ch).toMap
}

def solver(grid: Map[Point, Char]): (String, Int) = {
    var pos = grid.collectFirst { case (k, v) if k.y == 0 && v == '|' => k }.get
    var dir = Direction(1, 0)
    
    val msg = new StringBuilder
    var steps = 0

    while (grid.contains(pos)) {
        grid(pos) match {
            case '+' => {
                if (grid.contains(pos + dir.rotateLeft)) { dir = dir.rotateLeft }
                else if (grid.contains(pos + dir.rotateRight)) { dir = dir.rotateRight }
            }
            case ch if ch.isLetter => msg.append(ch)
            case _ => {}
        }

        pos += dir
        steps += 1
    }

    return (msg.toString, steps)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day19.txt") match {
        case Success(lines) => {
            val (partOne, partTwo) = solver(parseInput(lines))
            println(s"Part One: $partOne")
            println(s"Part Two: $partTwo")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}