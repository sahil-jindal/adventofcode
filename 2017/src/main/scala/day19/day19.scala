package day19

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Point(val x: Int, val y: Int)

def onPath(map: List[String], y: Int, x: Int): Boolean = {
    val (w, h) = (map(0).length, map.length)
    return (0 until w).contains(x) && (0 until h).contains(y) && map(y)(x) != ' '
}

def followPath(map: List[String]): (String, Int) = {
    var p = Point(map(0).indexOf('|'), 0)
    var d = Point(0, 1)
    
    val msg = new StringBuilder
    var steps = 0

    while onPath(map, p.y, p.x) do {
        map(p.y)(p.x) match {
            case '+' => {
                d = Seq(Point(d.y, -d.x), Point(-d.y, d.x)).collectFirst {
                    case Point(dx, dy) if onPath(map, p.y + dy, p.x + dx) => Point(dx, dy)
                }.getOrElse(Point(d.x, d.y))
            }
            case ch if ch.isLetter => msg.append(ch)
            case _ => {}
        }

        p = Point(p.x + d.x, p.y + d.y)
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