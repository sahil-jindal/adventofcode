package day24

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map, Set, PriorityQueue}

case class Point(x: Int, y: Int)
case class State(cost: Int, pos: Int, visited: Int)

// Direction Lists for movement
val directions = List(Point(0, 1), Point(1, 0), Point(0, -1), Point(-1, 0))

def parseInput(grid: List[String]): (Map[Int, Point], Set[Point]) = {
    val locations = Map.empty[Int, Point]
    val walls = Set.empty[Point]
    
    for (y <- grid.indices; x <- grid(y).indices) {
        grid(y)(x) match {
            case '#' => walls.add(Point(x, y))
            case d if d.isDigit => locations(d.asDigit) = Point(x, y)
            case _ =>
        }
    }
    
    return (locations, walls)
}

// A* Search to compute shortest paths between POIs
def aStar(start: Point, goal: Point, walls: Set[Point]): Int = {
    def evaluation(pair: (Int, Point)): Int = {
        val (costSoFar, current) = pair
        return costSoFar + (current.x - goal.x).abs + (current.y - goal.y).abs
    }
        
    val pq = PriorityQueue((0, start))(using Ordering.by(evaluation).reverse)
    val visited = Set.empty[Point]

    while (pq.nonEmpty) {
        val (costSoFar, current) = pq.dequeue()
        if (current == goal) return costSoFar
        if (!visited.contains(current)) {
            visited += current
            for (dir <- directions) {
                val neighbor = Point(current.x + dir.x, current.y + dir.y)
                if (!walls.contains(neighbor) && !visited.contains(neighbor)) {
                    pq.enqueue((costSoFar + 1, neighbor))
                }
            }
        }
    }

    return Int.MaxValue
}

// Precompute shortest distances between all numbered locations
def precomputeDistances(locations: Map[Int, Point], walls: Set[Point]): Map[(Int, Int), Int] = {
    val keys = locations.keys.toList
    val result = Map.empty[(Int, Int), Int]
    
    for { i <- keys; j <- keys if i != j } result((i, j)) = aStar(locations(i), locations(j), walls)
    
    return result
}

// A* for solving TSP using state-space search
def tspAStar(start: Int, numLocations: Int, dist: Map[(Int, Int), Int], returnToStart: Boolean): Int = {
    val pq = PriorityQueue.empty(using Ordering.by[State, Int](_.cost).reverse)
    val best = Map.empty[(Int, Int), Int].withDefaultValue(Int.MaxValue)
    var minCost = Int.MaxValue

    pq.enqueue(State(0, start, 1 << start))

    while (pq.nonEmpty) {
        val State(cost, pos, visited) = pq.dequeue()
        
        if (visited == (1 << numLocations) - 1) {
            val finalCost = if (returnToStart) cost + dist((pos, start)) else cost
            minCost = math.min(minCost, finalCost)
        } else {
            for (next <- 0 until numLocations if (visited & (1 << next)) == 0) {
                val newCost = cost + dist((pos, next))
                val newVisited = visited | (1 << next)
                
                if (newCost < best((next, newVisited))) {
                    best((next, newVisited)) = newCost
                    pq.enqueue(State(newCost, next, newVisited))
                }
            }
        }
    }

    return minCost
}

def evaluatorOne(noOfLocations: Int, dist: Map[(Int, Int), Int]): Int = tspAStar(0, noOfLocations, dist, false)
def evaluatorTwo(noOfLocations: Int, dist: Map[(Int, Int), Int]): Int = tspAStar(0, noOfLocations, dist, true)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day24.txt") match {
        case Success(lines) => {
            val (locations, walls) = parseInput(lines)
            val dist = precomputeDistances(locations, walls)
            println(s"Part One: ${evaluatorOne(locations.size, dist)}")
            println(s"Part Two: ${evaluatorTwo(locations.size, dist)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}