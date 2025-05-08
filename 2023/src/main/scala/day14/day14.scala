package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map, ListBuffer}

type Grid = List[Array[Char]]

def parseInput(input: List[String]) = input.map(_.toArray)

def deepCopy(grid: Grid): Grid = {
    return grid.map(row => row.clone())
}

 // Tilt the grid to the North, so that the 'O' tiles roll to the top.
def tilt(gridInit: Grid): Grid = {
    val grid = deepCopy(gridInit)

    for (x <- grid(0).indices) {
        var yT = 0 // tells where to roll up the next 'O' tile
        for (yS <- grid.indices) {
            if (grid(yS)(x) == '#') {
                yT = yS + 1
            } else if (grid(yS)(x) == 'O') {
                grid(yS)(x) = '.' 
                grid(yT)(x) = 'O' 
                yT += 1
            }
        }
    }

    return grid
}

// returns the cummulated distances of 'O' tiles from the bottom of the grid
def measure(grid: Grid): Int = grid.zipWithIndex.map { 
    case (row, y) => (row.length - y) * row.count(_ == 'O')
}.sum

def rotate(src: Grid): Grid = {
    val dist = Array.fill(src(0).length, src.length)(' ')

    for (y <- src(0).indices; x <- src.indices) {
        dist(y)(x) = src(src.length - x - 1)(y)
    }

    return dist.toList
}

def cycle(gridInit: Grid): Grid = {
    var grid = deepCopy(gridInit)

    for (_ <- 0 until 4) { grid = rotate(tilt(grid)) }

    return grid
}

def iterate(gridInit: Grid, countInit: Int): Grid = {
    // The usual trick: keep iterating until we find a loop, make a shortcut
    // and read the result from the accumulated history.

    val history = ListBuffer.empty[List[String]]
    val seen = Map.empty[List[String], Int]
    var grid = deepCopy(gridInit)
    var count = countInit

    while (count > 0) {
        grid = cycle(grid)
        count -= 1

        val currentKey = grid.map(_.toString)

        seen.get(currentKey) match {
            case Some(prevIndex) =>
                val loopLength = history.size - prevIndex
                val remainder = count % loopLength
                val targetIndex = prevIndex + remainder
                val targetKey = history(targetIndex)
                return targetKey.map(_.toArray)
            case None =>
                seen(currentKey) = history.size
                history += currentKey
        }
    }

    return grid
}

def evaluatorOne(input: Grid): Int = measure(tilt(input))
def evaluatorTwo(input: Grid): Int = measure(iterate(input, 1_000_000_000))

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