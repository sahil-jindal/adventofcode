package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map
import scala.util.control.Breaks._

type Grid = Array[Array[Char]]

def parseInput(input: List[String]) = input.map(_.toCharArray).toArray

// Tilt the platform north
def tiltNorth(platform: Grid): Grid = {
    val (height, width) = (platform.length, platform(0).length)
    val result = platform.map(_.clone())

    for (x <- 0 until width) {
        var nextEmptyRow = 0
        for (y <- 0 until height) {
            result(y)(x) match {
                case '#' => nextEmptyRow = y + 1
                case 'O' => {
                    result(y)(x) = '.'
                    result(nextEmptyRow)(x) = 'O'
                    nextEmptyRow += 1
                }
                case _ =>
            }
        }
    }

    return result
}

// Rotate the platform 90 degrees clockwise
def rotateClockwise(platform: Grid): Grid = {
    val (height, width) = (platform.length, platform(0).length)
    val rotated = Array.ofDim[Char](width, height)
    
    for (y <- 0 until height; x <- 0 until width) {
        rotated(x)(height - y - 1) = platform(y)(x)
    }
    
    return rotated
}

// One complete cycle: North, West, South, East
def cycle(platform: Grid): Grid = {
    var current = platform

    for (_ <- 0 until 4) { 
        current = rotateClockwise(tiltNorth(current)) 
    }

    return current
}

// Run the specified number of cycles, detecting cycles to avoid unnecessary computation
def runCycles(platform: Grid): Grid = {
    val numCycles = 1000000000
    var current = platform
    val seen = Map.empty[String, Int]
    
    breakable {
        for (i <- 0 until numCycles) {
            val platformKey = current.flatten.mkString
        
            if (seen.contains(platformKey)) {
                // Found a cycle, calculate the final state
                val cycleStart = seen(platformKey)
                val cycleLength = i - cycleStart
                val remainingCycles = (numCycles - i) % cycleLength
            
                // Fast-forward by running only the remaining cycles
                for (_ <- 0 until remainingCycles) {
                    current = cycle(current)
                }

                break()
            }
        
            seen(platformKey) = i
            current = cycle(current)
        }
    }
    
    return current
}

// Calculate the load on the north support beams
def calculateLoad(platform: Grid): Int = {
    return platform.reverseIterator.zipWithIndex.map { case (line, y) => 
        line.count(_ == 'O') * (y + 1)
    }.sum
}

def evaluatorOne(input: Grid): Int = calculateLoad(tiltNorth(input))
def evaluatorTwo(input: Grid): Int = calculateLoad(runCycles(input))

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day14.txt") match {
        case Success(lines) => {
            val input = parseInput(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}