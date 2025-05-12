package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map, ListBuffer}
import scala.util.boundary, boundary.break

type Grid = List[Array[Char]]

def parseInput(input: List[String]) = input.map(_.toCharArray).toArray

// Tilt the platform north
def tiltNorth(platform: Array[Array[Char]]): Array[Array[Char]] = {
    val rows = platform.length
    val cols = platform(0).length
    val result = platform.map(_.clone())
    
    for (col <- 0 until cols) {
        var nextEmptyRow = 0
        for (row <- 0 until rows) {
            result(row)(col) match {
                case 'O' => 
                    if (nextEmptyRow < row) {
                        // Move the rounded rock to the empty spot
                        result(nextEmptyRow)(col) = 'O'
                        result(row)(col) = '.'
                        nextEmptyRow += 1
                    } else {
                        nextEmptyRow = row + 1
                    }
                case '#' => 
                    // Cube-shaped rocks don't move, but block movement
                    nextEmptyRow = row + 1
                case '.' => 
                    // Do nothing for empty spaces
            }
        }
    }
    
    result
}
  
// Tilt the platform south
def tiltSouth(platform: Array[Array[Char]]): Array[Array[Char]] = {
    val rows = platform.length
    val cols = platform(0).length
    val result = platform.map(_.clone())
    
    for (col <- 0 until cols) {
        var nextEmptyRow = rows - 1
        for (row <- (rows - 1) to 0 by -1) {
            result(row)(col) match {
                case 'O' => 
                    if (nextEmptyRow > row) {
                        // Move the rounded rock to the empty spot
                        result(nextEmptyRow)(col) = 'O'
                        result(row)(col) = '.'
                        nextEmptyRow -= 1
                    } else {
                        nextEmptyRow = row - 1
                    }
                case '#' => 
                    // Cube-shaped rocks don't move, but block movement
                    nextEmptyRow = row - 1
                case '.' => 
                    // Do nothing for empty spaces
            }
        }
    }
    
    result
}
  
// Tilt the platform west
def tiltWest(platform: Array[Array[Char]]): Array[Array[Char]] = {
    val rows = platform.length
    val cols = platform(0).length
    val result = platform.map(_.clone())
    
    for (row <- 0 until rows) {
        var nextEmptyCol = 0
        for (col <- 0 until cols) {
            result(row)(col) match {
                case 'O' => 
                    if (nextEmptyCol < col) {
                        // Move the rounded rock to the empty spot
                        result(row)(nextEmptyCol) = 'O'
                        result(row)(col) = '.'
                        nextEmptyCol += 1
                    } else {
                        nextEmptyCol = col + 1
                    }
                case '#' => 
                    // Cube-shaped rocks don't move, but block movement
                    nextEmptyCol = col + 1
                case '.' => 
                    // Do nothing for empty spaces
            }
        }
    }
    
    result
}
  
// Tilt the platform east
def tiltEast(platform: Array[Array[Char]]): Array[Array[Char]] = {
    val rows = platform.length
    val cols = platform(0).length
    val result = platform.map(_.clone())
    
    for (row <- 0 until rows) {
        var nextEmptyCol = cols - 1
        for (col <- (cols - 1) to 0 by -1) {
            result(row)(col) match {
                case 'O' => 
                    if (nextEmptyCol > col) {
                        // Move the rounded rock to the empty spot
                        result(row)(nextEmptyCol) = 'O'
                        result(row)(col) = '.'
                        nextEmptyCol -= 1
                    } else {
                        nextEmptyCol = col - 1
                    }
                case '#' => 
                    // Cube-shaped rocks don't move, but block movement
                    nextEmptyCol = col - 1
                case '.' => 
                    // Do nothing for empty spaces
            }
        }
    }
    
    result
}
  
// Calculate the load on the north support beams
def calculateLoad(platform: Array[Array[Char]]): Int = {
    val rows = platform.length
    
    return (0 until rows).map { row =>
        platform(row).count(_ == 'O') * (rows - row)
    }.sum
}
  
// One complete cycle: North, West, South, East
def cycle(platform: Array[Array[Char]]): Array[Array[Char]] = {
    val afterNorth = tiltNorth(platform)
    val afterWest = tiltWest(afterNorth)
    val afterSouth = tiltSouth(afterWest)
    tiltEast(afterSouth)
}
  
// Run the specified number of cycles, detecting cycles to avoid unnecessary computation
def runCycles(platform: Array[Array[Char]], numCycles: Int): Int = {
    var current = platform
    val seen = Map.empty[String, Int]
    
    boundary {
        for (i <- 0 until numCycles) {
            val platformKey = current.map(_.mkString).mkString("\n")
        
            if (seen.contains(platformKey)) {
                // Found a cycle, calculate the final state
                val cycleStart = seen(platformKey)
                val cycleLength = i - cycleStart
                val remainingCycles = (numCycles - i) % cycleLength
            
                // Fast-forward by running only the remaining cycles
                for (_ <- 0 until remainingCycles) {
                    current = cycle(current)
                }

                break(calculateLoad(current))
            }
        
            seen(platformKey) = i
            current = cycle(current)
        }
    
        calculateLoad(current)
    }
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day14.txt") match {
        case Success(lines) => {
            val examplePlatform = parseInput(lines)
            val tiltedPlatform = tiltNorth(examplePlatform)
            println(s"Part One: ${calculateLoad(tiltedPlatform)}")
            println(s"Part Two: ${runCycles(examplePlatform, 1000000000)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}