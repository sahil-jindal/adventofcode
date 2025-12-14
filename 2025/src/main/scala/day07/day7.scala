package day07

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.BigInt

def evaluatorOne(grid: List[String]): Int = {
    val n = grid.length
    val m = grid.head.length

    // Find S
    val startR = grid.indexWhere(_.contains('S'))
    val startC = grid(startR).indexOf('S')

    var splits = 0
    var active = Set(startC) // beams entering the next row

    // Process rows below S
    for (r <- startR + 1 until n if active.nonEmpty) {
        var nextActive = Set.empty[Int]
        var toProcess = active
        var processed = Set.empty[Int]

        while (toProcess.nonEmpty) {
            val col = toProcess.head
            toProcess -= col

            if (!processed(col) && col >= 0 && col < m) {
                processed += col
                
                grid(r)(col) match {
                    case '^' => {
                        // Split the beam
                        splits += 1
                        val left = col - 1
                        val right = col + 1

                        // Add new beams on same row if not already processed
                        if (!processed(left) && left >= 0) toProcess += left
                        if (!processed(right) && right < m) toProcess += right
                    }
                    case _ => {
                        // Beam passes through to next row
                        nextActive += col
                    }
                }
            }
        }

        active = nextActive
    }

    return splits
}

def evaluatorTwo(grid: List[String]): BigInt = {
    val n = grid.length
    val m = grid.head.length

    // find S
    val startR = grid.indexWhere(_.contains('S'))
    val startC = grid(startR).indexOf('S')

    // active counts entering the next row: column -> count of timelines
    var active = mutable.Map(startC -> BigInt(1))
    var finished = BigInt(0)

    // process rows below S

    for (r <- startR + 1 until n if active.nonEmpty) {
        // current row counts (we'll mutate this while resolving split cascades on this same row)
        val curr = active.clone()//.withDefaultValue(BigInt(0))

        // Resolve splits on this same row until no splitter positions remain with counts
        var changed = true
        
        while (changed) {
            changed = false
            
            // Collect positions that are splitters and have a positive count
            val splitCols = curr.keys.filter { col =>
                col >= 0 && col < m && grid(r)(col) == '^' && curr(col) > 0
            }.toList

            if (splitCols.nonEmpty) {
                changed = true
                
                for (col <- splitCols) {
                    val count = curr.remove(col).getOrElse(BigInt(0))
                    // emit to left and right on same row
                    val left = col - 1
                    val right = col + 1
                    if (left >= 0 && left < m) curr(left) = curr.getOrElse(left, BigInt(0)) + count
                    // if left out of bounds -> those timelines are lost
                    if (right >= 0 && right < m) curr(right) = curr.getOrElse(right, BigInt(0)) + count
                    // splitter consumed (so removed from curr)
                }
            }
        }

        // After row stabilizes, timelines on non-splitter cells move down to next row
        val nextActive = mutable.Map.empty[Int, BigInt]
        
        for ((col, cnt) <- curr) {
            if (col < 0 || col >= m) {
                // out of bounds - lost
            } else if (r == n - 1) {
                // This is the last row; when they move down they exit the grid => finished
                finished += cnt
            } else {
                // If the cell is a splitter on this row it would have been removed earlier, so this is safe
                nextActive(col) = nextActive.getOrElse(col, BigInt(0)) + cnt
            }
        }

        active = nextActive
    }

    // If after finishing all rows we still have active timelines (this can happen if S is at last row),
    // they will exit immediately (move down out of grid)
    if (active.nonEmpty) {
        finished += active.values.sum
    }

    return finished
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day07.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}