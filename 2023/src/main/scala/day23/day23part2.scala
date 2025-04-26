package day23

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable

val dirs = Vector((0, 1), (1, 0), (0, -1), (-1, 0))
  
// Slopes mapping
val slopes = Map(
    '>' -> 0, // right
    'v' -> 1, // down
    '<' -> 2, // left
    '^' -> 3  // up
)

def getValidNeighbors(grid: List[String], row: Int, col: Int, respectSlopes: Boolean): List[(Int, Int)] = {
    val height = grid.length
    val width = grid(0).length
    
    if (grid(row)(col) == '#') return List()
    
    val currentCell = grid(row)(col)
    
    if (respectSlopes && slopes.contains(currentCell)) {
        // If we're respecting slopes, only move in the direction of the slope
        val dir = dirs(slopes(currentCell))
        val newRow = row + dir._1
        val newCol = col + dir._2
      
        if (newRow >= 0 && newRow < height && newCol >= 0 && newCol < width && grid(newRow)(newCol) != '#') {
            return List((newRow, newCol))
        }
        
        return List()
    } else {
        // Otherwise, try all four directions
        val neighbors = for {
            (dr, dc) <- dirs
            newRow = row + dr
            newCol = col + dc
            if newRow >= 0 && newRow < height && newCol >= 0 && newCol < width && grid(newRow)(newCol) != '#'
            if !respectSlopes || !slopes.contains(grid(newRow)(newCol)) || slopes(grid(newRow)(newCol)) != (dirs.indexOf((dr, dc)) + 2) % 4
        } yield (newRow, newCol)
      
        neighbors.toList
    }
}

def findJunctions(grid: List[String], start: (Int, Int), end: (Int, Int), respectSlopes: Boolean): Set[(Int, Int)] = {
    val height = grid.length
    val width = grid(0).length
    
    val junctions = mutable.Set[((Int, Int))]()
    junctions += start
    junctions += end
    
    for (r <- 0 until height; c <- 0 until width) {
        if (grid(r)(c) != '#') {
            val neighbors = getValidNeighbors(grid, r, c, respectSlopes)
            if (neighbors.size > 2) {
                junctions += ((r, c))
            }
        }
    }
    
    junctions.toSet
}

def findEdges(grid: List[String], start: (Int, Int), junctions: Set[(Int, Int)], respectSlopes: Boolean): List[((Int, Int), Int)] = {
    val queue = mutable.Queue(((start, 0)))
    val visited = mutable.Set(start)
    val edges = mutable.ListBuffer[((Int, Int), Int)]()
    
    while (queue.nonEmpty) {
        val ((row, col), distance) = queue.dequeue()
      
        if (distance > 0 && junctions.contains((row, col))) {
            edges += (((row, col), distance))
        } else {
            for {
                neighbor @ (nr, nc) <- getValidNeighbors(grid, row, col, respectSlopes)
                if !visited.contains(neighbor)
            } {
                visited += neighbor
                queue.enqueue((neighbor, distance + 1))
            }
        }
    }
    
    edges.toList
}

def buildGraph(grid: List[String], junctions: Set[(Int, Int)], respectSlopes: Boolean): Map[(Int, Int), List[((Int, Int), Int)]] = {
    val graph = mutable.Map[(Int, Int), List[((Int, Int), Int)]]()
    
    for (junction <- junctions) {
        val edges = findEdges(grid, junction, junctions, respectSlopes)
        graph(junction) = edges
    }
    
    graph.toMap
}

def findLongestPathInGraph(graph: Map[(Int, Int), List[((Int, Int), Int)]], start: (Int, Int), end: (Int, Int)): Int = {
    def dfs(current: (Int, Int), visited: Set[(Int, Int)]): Int = {
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
    
    dfs(start, Set())
}

def findLongestPath(grid: List[String], respectSlopes: Boolean): Int = {
    val height = grid.length
    val width = grid(0).length
    
    val start = (0, grid(0).indexOf('.'))
    val end = (height - 1, grid(height - 1).indexOf('.'))
    
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