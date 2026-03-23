package day24

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{PriorityQueue, Set => MutableSet}

case class Point(y: Int, x: Int)

type Input = (locations: Map[Int, Point], walls: Set[Point])

def parseInput(grid: List[String]): Input = {
    val pairs = (for {
        (line, y) <- grid.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield ch -> Point(y, x))
    
    val walls = pairs.collect { case ('#', pt) => pt }.toSet

    val locations = (for {
        (d, pt) <- pairs
        if d.isDigit
    } yield d.asDigit -> pt).toMap
    
    return (locations, walls)
}

def getNeighbours(pos: Point) = List(
    pos.copy(x = pos.x + 1),
    pos.copy(x = pos.x - 1),
    pos.copy(y = pos.y + 1),
    pos.copy(y = pos.y - 1)
)

// A* Search to compute shortest paths between POIs
def aStar(start: Point, goal: Point, walls: Set[Point]): Int = {
    def evaluation(pair: (Int, Point)): Int = {
        val (costSoFar, current) = pair
        return costSoFar + (current.x - goal.x).abs + (current.y - goal.y).abs
    }
        
    val pq = PriorityQueue((0, start))(using Ordering.by(evaluation).reverse)
    val seen = MutableSet.empty[Point]

    while (pq.nonEmpty) {
        val (costSoFar, current) = pq.dequeue()

        if (current == goal) return costSoFar

        if (seen.add(current)) {
            for (neighbor <- getNeighbours(current)) {
                if (!walls.contains(neighbor) && !seen.contains(neighbor)) {
                    pq.enqueue((costSoFar + 1, neighbor))
                }
            }
        }
    }

    return Int.MaxValue
}

def solver(input: Input): (Int, Int) = {
    val (locations, walls) = input

    val dist = (for {
        (i, p1) <- locations
        (j, p2) <- locations - i
    } yield (i, j) -> aStar(p1, p2, walls)).toMap 
    
    require(locations.contains(0))

    val remaining = (locations.keySet - 0).toSeq
    var (partOne, partTwo) = (Int.MaxValue, Int.MaxValue)

    for (slice <- remaining.permutations) {
        val first = dist((0, slice.head))
        val middle = (slice.init zip slice.tail).map(dist).sum
        val last = dist((slice.last, 0))

        partOne = partOne.min(first + middle)
        partTwo = partTwo.min(first + middle + last)
    }

    return (partOne, partTwo)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day24.txt") match {
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