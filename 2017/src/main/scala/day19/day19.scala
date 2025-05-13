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

def onPath(map: List[String], p: Point): Boolean = {
    val (w, h) = (map(0).length, map.length)
    return (0 until w).contains(p.x) && (0 until h).contains(p.y) && map(p.y)(p.x) != ' '
}

def followPath(map: List[String]): (String, Int) = {
    var pos = Point(0, map(0).indexOf('|'))
    var dir = Direction(1, 0)
    
    val msg = new StringBuilder
    var steps = 0

    while (onPath(map, pos)) {
        map(pos.y)(pos.x) match {
            case '+' => {
                if (onPath(map, pos + dir.rotateLeft)) then { dir = dir.rotateLeft }
                else if (onPath(map, pos + dir.rotateRight)) then { dir = dir.rotateRight }
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
            val (msg, steps) = followPath(lines)
            println(s"Part One: $msg")
            println(s"Part Two: $steps")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}