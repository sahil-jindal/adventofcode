package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, ListBuffer}

case class Point(y: Int, x: Int)

type Grid = Map[Point, Int]

def parseInput(input: List[String]): Grid = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Point(y, x) -> ch.asDigit).toMap
}

def getNeighbours(pos: Point) = List(
    pos.copy(x = pos.x - 1),
    pos.copy(x = pos.x + 1),
    pos.copy(y = pos.y - 1),
    pos.copy(y = pos.y + 1),
)

def getTrailsFrom(grid: Grid, trailHead: Point): List[Point] = {
    val positions = Queue(trailHead)
    val trails = ListBuffer.empty[Point]

    while (positions.nonEmpty) {
        val point = positions.dequeue()

        if (grid(point) == 9) {
            trails += point
        } else {
            for (newPos <- getNeighbours(point).filter(grid.contains)) {
                if (grid(newPos) == grid(point) + 1) {
                    positions.enqueue(newPos)
                }
            }
        }
    }

    return trails.toList
}

def evaluatorOne(allTrails: List[List[Point]]): Int = allTrails.flatMap(_.distinct).size
def evaluatorTwo(allTrails: List[List[Point]]): Int = allTrails.flatten.size

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day10.txt") match {
        case Success(lines) => {
            val grid = parseInput(lines)
            val trailHeads = grid.collect { case (pos, ch) if ch == 0 => pos }.toList
            val allTrails = trailHeads.map(pos => getTrailsFrom(grid, pos))
            println(s"Part One: ${evaluatorOne(allTrails)}")
            println(s"Part Two: ${evaluatorTwo(allTrails)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}