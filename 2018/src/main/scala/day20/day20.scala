package day20

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, Stack, Set}

case class Point(x: Int, y: Int) {
    def move(dx: Int, dy: Int): Point = Point(x + dx, y + dy)
}

def doors(input: String): List[(Point, Point)] = {
    val s = Stack.empty[Point]
    var pos = Point(0, 0)
    
    return input.collect {
        case 'N' => val prev = pos; pos = pos.move(0, -1); Seq((prev, pos), (pos, prev))
        case 'S' => val prev = pos; pos = pos.move(0, 1); Seq((prev, pos), (pos, prev))
        case 'E' => val prev = pos; pos = pos.move(1, 0); Seq((prev, pos), (pos, prev))
        case 'W' => val prev = pos; pos = pos.move(-1, 0); Seq((prev, pos), (pos, prev))
        case '(' => s.push(pos); Seq.empty
        case '|' => pos = s.top; Seq.empty
        case ')' => pos = s.pop(); Seq.empty
    }.flatten.toList
}

def solver(input: String): (Int, Int) = {
    val grid = doors(input).groupMap(_._1)(_._2)

    val queue = Queue((Point(0, 0), 0))
    val seen = Set.empty[Point]

    var (dMax, distantRooms) = (Int.MinValue, 0)

    while (queue.nonEmpty) {
        val (pos, d) = queue.dequeue()
        
        if (!seen.contains(pos)) {
            dMax = math.max(dMax, d)
            if (d >= 1000) distantRooms += 1

            seen.add(pos)
            grid.getOrElse(pos, List()).foreach(nextPos => queue.enqueue((nextPos, d + 1)))
        }
    }

    return (dMax, distantRooms)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day20.txt") match {
        case Success(lines) => {
            val (dMax, distantRooms) = solver(lines.head)
            println(s"Part One: $dMax")
            println(s"Part Two: $distantRooms")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}