package day09

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, Set}

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
    pos.copy(y = pos.y + 1)
)

def getLowPoints(grid: Grid) = grid.keys.filter(pos =>
    getNeighbours(pos).filter(grid.contains).forall(nbr => grid(nbr) > grid(pos))
).toList

def basicInSize(grid: Grid, point: Point): Int = {
    val queue = Queue(point)
    val filled = Set(point)

    while (queue.nonEmpty) {
        val current = queue.dequeue()
        
        for (nbr <- getNeighbours(current).filter(grid.contains)) {
            if (!filled.contains(nbr) && grid(nbr) != 9) {
                filled.add(nbr)
                queue.enqueue(nbr)
            }
        }
    }

    return filled.size
}

def evaluatorOne(grid: Grid): Int = {
    return getLowPoints(grid).map(point => 1 + grid(point)).sum
}

def evaluatorTwo(grid: Grid): Int = {
    return getLowPoints(grid)
        .map(p => basicInSize(grid, p))
        .sorted(using Ordering.Int.reverse)
        .take(3)
        .product
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day09.txt") match {
        case Success(lines) => {
            val input = parseInput(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exceposion) => {
            println(s"Error reading file: ${exceposion.getMessage}")
        }
    }
}