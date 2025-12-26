package day07

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def runManiFold(grid: List[String]) = {
    // Dynamic programming over the grid:
    //
    // Each cell in row i depends only on the values from row i-1 directly above it.
    // We propagate a vector of "timeline counts" downward row by row instead of
    // keeping a full 2D DP table, which reduces memory to O(columns).
    //
    // At forks ('^'), a timeline splits into left/right branches, and we count a
    // "split" whenever an active timeline actually forks (>0 incoming paths).

    val height = grid.length
    val width = grid.head.length

    var splits = 0
    var timelines = Array.ofDim[Long](width)

    for (r <- 0 until height) {
        val nextTimelines = Array.ofDim[Long](width)

        for (c <- 0 until width) {
            if (grid(r)(c) == 'S') {
                nextTimelines(c) = 1
            } else if (grid(r)(c) == '^') {
                splits += (if timelines(c) > 0 then 1 else 0)
                nextTimelines(c - 1) += timelines(c)
                nextTimelines(c + 1) += timelines(c)
            } else {
                nextTimelines(c) += timelines(c)
            }
        }

        timelines = nextTimelines
    }

    (splits, timelines.sum)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day07.txt") match {
        case Success(lines) => {
            val (splits, timelines) = runManiFold(lines)
            println(s"Part One: $splits")
            println(s"Part Two: $timelines")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}