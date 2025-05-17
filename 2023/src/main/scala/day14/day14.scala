package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map, ListBuffer}
import scala.util.boundary, boundary.break

type Grid = List[Array[Char]]

def parseInput(input: List[String]) = input.map(_.toCharArray).toArray

// Tilt the platform north
def tiltNorth(platform: Array[Array[Char]]): Array[Array[Char]] = {
    val height = platform.length
    val width = platform(0).length
    val result = platform.map(_.clone())
    
    for (x <- 0 until width) {
        var nextEmptyRow = 0
        for (y <- 0 until height) {
            result(y)(x) match {
                case 'O' => 
                    if (nextEmptyRow < y) {
                        // Move the rounded rock to the empty spot
                        result(nextEmptyRow)(x) = 'O'
                        result(y)(x) = '.'
                        nextEmptyRow += 1
                    } else {
                        nextEmptyRow = y + 1
                    }
                case '#' => 
                    // Cube-shaped rocks don't move, but block movement
                    nextEmptyRow = y + 1
                case '.' => 
                    // Do nothing for empty spaces
            }
        }
    }
    
    result
}
  
// Tilt the platform south
def tiltSouth(platform: Array[Array[Char]]): Array[Array[Char]] = {
    val height = platform.length
    val width = platform(0).length
    val result = platform.map(_.clone())
    
    for (x <- 0 until width) {
        var nextEmptyRow = height - 1
        for (y <- (height - 1) to 0 by -1) {
            result(y)(x) match {
                case 'O' => 
                    if (nextEmptyRow > y) {
                        // Move the rounded rock to the empty spot
                        result(nextEmptyRow)(x) = 'O'
                        result(y)(x) = '.'
                        nextEmptyRow -= 1
                    } else {
                        nextEmptyRow = y - 1
                    }
                case '#' => 
                    // Cube-shaped rocks don't move, but block movement
                    nextEmptyRow = y - 1
                case '.' => 
                    // Do nothing for empty spaces
            }
        }
    }
    
    result
}
  
// Tilt the platform west
def tiltWest(platform: Array[Array[Char]]): Array[Array[Char]] = {
    val height = platform.length
    val width = platform(0).length
    val result = platform.map(_.clone())
    
    for (y <- 0 until height) {
        var nextEmptyCol = 0
        for (x <- 0 until width) {
            result(y)(x) match {
                case 'O' => 
                    if (nextEmptyCol < x) {
                        // Move the rounded rock to the empty spot
                        result(y)(nextEmptyCol) = 'O'
                        result(y)(x) = '.'
                        nextEmptyCol += 1
                    } else {
                        nextEmptyCol = x + 1
                    }
                case '#' => 
                    // Cube-shaped rocks don't move, but block movement
                    nextEmptyCol = x + 1
                case '.' => 
                    // Do nothing for empty spaces
            }
        }
    }
    
    result
}
  
// Tilt the platform east
def tiltEast(platform: Array[Array[Char]]): Array[Array[Char]] = {
    val height = platform.length
    val width = platform(0).length
    val result = platform.map(_.clone())
    
    for (y <- 0 until height) {
        var nextEmptyCol = width - 1
        for (x <- (width - 1) to 0 by -1) {
            result(y)(x) match {
                case 'O' => 
                    if (nextEmptyCol > x) {
                        // Move the rounded rock to the empty spot
                        result(y)(nextEmptyCol) = 'O'
                        result(y)(x) = '.'
                        nextEmptyCol -= 1
                    } else {
                        nextEmptyCol = x - 1
                    }
                case '#' => 
                    // Cube-shaped rocks don't move, but block movement
                    nextEmptyCol = x - 1
                case '.' => 
                    // Do nothing for empty spaces
            }
        }
    }
    
    result
}
  
// Calculate the load on the north support beams
def calculateLoad(platform: Array[Array[Char]]): Int = {
    val height = platform.length
    
    return (0 until height).map { y =>
        platform(y).count(_ == 'O') * (height - y)
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