package day07

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

//! Examining the input shows that it consists of a triangular Christmas tree shape with both every
//! second line and second space blank. Two splitters will never occur immediately next to each
//! other. This structure speeds up and simplifies the solution, and we compute both parts together.
//!
//! The key insight to part two is that we only need the *total count* of paths, not each
//! separate path. This means that if 2 paths enter a tile from the left and another 2 from the
//! right, then we can simply sum the paths to 4. A dynamic programming approach counts the total
//! number of paths one row at a time.
//!
//! When a beam hits a splitter, the count underneath the splitter will be zero, and the number
//! of beams to either side is incremented by the count of the beams hitting the splitter.

def runManiFold(grid: List[String]): (Int, Long) = {
    val width = grid.head.length
    val center = width >> 1

    val positions = (for {
        (line, idx) <- grid.drop(2).zipWithIndex
        if (idx & 1) == 0
        y = idx >> 1
        x <- center - y to center + y by 2
        if line(x) == '^'
    } yield x)

    var splits = 0
    val timelines = Array.ofDim[Long](width)
    timelines(center) = 1

    for (x <- positions) {
        val count = timelines(x)

        if (count > 0) {
            splits += 1
            timelines(x) = 0
            timelines(x - 1) += count
            timelines(x + 1) += count
        }
    }

    return (splits, timelines.sum)
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