package day23

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, ListBuffer, Map, Set => MutableSet}

case class Position(y: Int, x: Int)

val dirs = Vector((0, 1), (1, 0), (0, -1), (-1, 0))
  
// Slopes mapping
val slopes = Map(
    '>' -> 0, // right
    'v' -> 1, // down
    '<' -> 2, // left
    '^' -> 3  // up
)

def getValidNeighbors(grid: List[String], pos: Position, respectSlopes: Boolean): List[Position] = {
    val height = grid.length
    val width = grid(0).length
    
    if (grid(pos.y)(pos.x) == '#') return List.empty
    
    val currentCell = grid(pos.y)(pos.x)
    
    if (respectSlopes && slopes.contains(currentCell)) {
        // If we're respecting slopes, only move in the direction of the slope
        val dir = dirs(slopes(currentCell))
        val newPosY = pos.y + dir._1
        val newPosX = pos.x + dir._2
      
        if (newPosY >= 0 && newPosY < height && newPosX >= 0 && newPosX < width && grid(newPosY)(newPosX) != '#') {
            return List(Position(newPosY, newPosX))
        }
        
        return List.empty
    } else {
        // Otherwise, try all four directions
        val neighbors = for {
            (dr, dc) <- dirs
            newPosY = pos.y + dr
            newPosX = pos.x + dc
            if newPosY >= 0 && newPosY < height && newPosX >= 0 && newPosX < width && grid(newPosY)(newPosX) != '#'
            if !respectSlopes || !slopes.contains(grid(newPosY)(newPosX)) || slopes(grid(newPosY)(newPosX)) != (dirs.indexOf((dr, dc)) + 2) % 4
        } yield Position(newPosY, newPosX)
      
        neighbors.toList
    }
}

def findJunctions(grid: List[String], start: Position, end: Position, respectSlopes: Boolean): Set[Position] = {
    val height = grid.length
    val width = grid(0).length
    
    val junctions = MutableSet.empty[(Position)]
    junctions += start
    junctions += end
    
    for (r <- 0 until height; c <- 0 until width) {
        if (grid(r)(c) != '#') {
            val neighbors = getValidNeighbors(grid, Position(r, c), respectSlopes)
            if (neighbors.size > 2) {
                junctions += Position(r, c)
            }
        }
    }
    
    junctions.toSet
}

def findEdges(grid: List[String], start: Position, junctions: Set[Position], respectSlopes: Boolean): List[(Position, Int)] = {
    val queue = Queue(((start, 0)))
    val visited = MutableSet(start)
    val edges = ListBuffer.empty[(Position, Int)]
    
    while (queue.nonEmpty) {
        val (pos, distance) = queue.dequeue()
      
        if (distance > 0 && junctions.contains(pos)) {
            edges += ((pos, distance))
        } else {
            for {
                neighbor @ newPos <- getValidNeighbors(grid, pos, respectSlopes)
                if !visited.contains(neighbor)
            } {
                visited += neighbor
                queue.enqueue((neighbor, distance + 1))
            }
        }
    }
    
    edges.toList
}

def buildGraph(grid: List[String], junctions: Set[Position], respectSlopes: Boolean): Map[Position, List[(Position, Int)]] = {
    val graph = Map.empty[Position, List[(Position, Int)]]
    
    for (junction <- junctions) {
        val edges = findEdges(grid, junction, junctions, respectSlopes)
        graph(junction) = edges
    }
    
    graph
}

def findLongestPathInGraph(graph: Map[Position, List[(Position, Int)]], start: Position, end: Position): Int = {
    def dfs(current: Position, visited: Set[Position]): Int = {
        if (current == end) return 0
      
        var maxLength = Int.MinValue
      
        for {
            (next, distance) <- graph(current)
            if !visited.contains(next)
        } {
            val length = dfs(next, visited + current)
            if (length != Int.MinValue) {
                maxLength = maxLength.max(length + distance)
            }
        }
      
        maxLength
    }
    
    dfs(start, Set.empty)
}

def findLongestPath(grid: List[String], respectSlopes: Boolean): Int = {
    val height = grid.length
    val width = grid(0).length
    
    val start = Position(0, grid(0).indexOf('.'))
    val end = Position(height - 1, grid(height - 1).indexOf('.'))
    
    // Find all junctions (positions with more than 2 neighbors)
    val junctions = findJunctions(grid, start, end, respectSlopes)
    
    // Build a graph of distances between junctions
    val graph = buildGraph(grid, junctions, respectSlopes)
    
    // Find the longest path in the graph
    findLongestPathInGraph(graph, start, end)
}

def readLinesFromFilePartTwo(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def helloPartTwo(): Unit = {
    readLinesFromFilePartTwo("day23.txt") match {
        case Success(lines) => println(s"Part Two: ${findLongestPath(lines, false)}")
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}