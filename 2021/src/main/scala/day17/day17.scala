package day17

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

type Input = (Int, Int, Int, Int)

def parseInput(input: String): Input = {
    val Seq(xMin, xMax, yMin, yMax) = raw"(-?\d+)".r.findAllIn(input).map(_.toInt).toSeq
    return (xMin, xMax, yMin, yMax)
}

def evaluatorOne(input: Input): Int = {
    val (_, _, bottom, _) = input
    val n = -(bottom + 1)
    return (n * (n + 1)) / 2
}

def evaluatorTwo(input: Input): Int = {
    val (left, right, bottom, top) = input

    // Minimum dx: smallest n where triangular number n*(n+1)/2 >= left
    val minDx = (1 until left).find(n => n * (n + 1) / 2 >= left).get
    val maxDx = right + 1
    val minDy = bottom
    val maxDy = -bottom
    val maxT  = (1 - 2 * bottom)

    val newArr = Array.fill(maxT)(0)
    val continuing = Array.fill(maxT)(0)

    for (initDx <- minDx until maxDx) {
        var x = 0
        var dx = initDx
        var first = true

        for (t <- 0 until maxT if x <= right) {
            if (x >= left) {
                if (first) {
                    first = false
                    newArr(t) += 1
                } else {
                    continuing(t) += 1
                }
            }
            
            x  += dx
            dx = (dx - 1).max(0)
        }
    }

    var total = 0

    for (initDy <- minDy until maxDy) {
        var y = 0
        var dy = initDy
        var t = 0
        var first = true

        while (y >= bottom) {
            if (y <= top) {
                if (first) {
                    first  = false
                    total += continuing(t)
                }
                
                total += newArr(t)
            }
            
            y += dy
            dy -= 1
            t += 1
        }
    }

    return total
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day17.txt") match {
        case Success(lines) => {
            val input = parseInput(lines.head)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}