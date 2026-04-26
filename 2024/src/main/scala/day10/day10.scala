package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, Map => MutableMap}

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

def getTrailsFrom(grid: Grid, trailHead: Point): List[Int] = {
    val positions = Queue(trailHead)
    val scoreMap = MutableMap.empty[Point, Int].withDefaultValue(0)

    while (positions.nonEmpty) {
        val pos = positions.dequeue()

        if (grid(pos) == 9) {
            scoreMap(pos) += 1
        } else {
            for (newPos <- getNeighbours(pos).filter(grid.contains)) {
                if (grid(newPos) == grid(pos) + 1) {
                    positions.enqueue(newPos)
                }
            }
        }
    }

    return scoreMap.values.toList
}

def preComputation(grid: Grid): List[Int] = {
    val trailHeads = grid.collect { case (pos, ch) if ch == 0 => pos }.toList
    return trailHeads.flatMap(pos => getTrailsFrom(grid, pos))
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day10.txt") match {
        case Success(lines) => {
            val input = preComputation(parseInput(lines))
            println(s"Part One: ${input.size}")
            println(s"Part Two: ${input.sum}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}