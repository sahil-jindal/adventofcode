package day20

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Stack, Queue, Set}

case class Point(y: Int, x: Int) {
    def move(dy: Int, dx: Int) = Point(y + dy, x + dx)
}

case class Pair(posFrom: Point, posTo: Point)

def doors(input: String): IndexedSeq[Pair] = {
    val s = Stack.empty[Point]
    var pos = Point(0, 0)
    
    return input.collect {
        case 'N' => val prev = pos; pos = pos.move(-1, 0); List(Pair(prev, pos), Pair(pos, prev))
        case 'S' => val prev = pos; pos = pos.move(1, 0); List(Pair(prev, pos), Pair(pos, prev))
        case 'E' => val prev = pos; pos = pos.move(0, 1); List(Pair(prev, pos), Pair(pos, prev))
        case 'W' => val prev = pos; pos = pos.move(0, -1); List(Pair(prev, pos), Pair(pos, prev))
        case '(' => s.push(pos); List.empty
        case '|' => pos = s.top; List.empty
        case ')' => pos = s.pop(); List.empty
    }.flatten
}

def solver(input: String): (Int, Int) = {
    val grid = doors(input).groupMap(_.posFrom)(_.posTo)

    val queue = Queue((Point(0, 0), 0))
    val seen = Set.empty[Point]

    var (dMax, distantRooms) = (Int.MinValue, 0)

    while (queue.nonEmpty) {
        val (pos, d) = queue.dequeue()
        
        if (seen.add(pos) && grid.contains(pos)) {
            dMax = math.max(dMax, d)
            
            if (d >= 1000) distantRooms += 1

            for (nextPos <- grid(pos)) {
                queue.enqueue((nextPos, d + 1))
            }
        }
    }

    return (dMax, distantRooms)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day20.txt") match {
        case Success(lines) => {
            val (partOne, partTwo) = solver(lines.head)
            println(s"Part One: $partOne")
            println(s"Part Two: $partTwo")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}