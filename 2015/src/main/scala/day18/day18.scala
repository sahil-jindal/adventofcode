package day18

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

type Grid = List[Array[Boolean]]

def parseInput(input: List[String]): Grid = {
    return input.map(_.collect {
        case '#' => true
        case '.' => false
    }.toArray)
}

def haloPaddingGrid(input: Grid): Grid = {
    val (h, w) = (input.length, input(0).length)
    val grid = Array.ofDim[Boolean](h + 2, w + 2)

    for (y <- 0 until h; x <- 0 until w) {
        grid(y + 1)(x + 1) = input(y)(x)
    }

    return grid.toList
}

def lightCondition(grid: Grid, y: Int, x: Int): Boolean = {
    val neighbours = (for {
        dy <- -1 to 1
        dx <- -1 to 1
        if dy != 0 || dx != 0
    } yield grid(y + dy)(x + dx))
    
    val valid = neighbours.count(identity)
    return valid == 3 || (grid(y)(x) && valid == 2)
}

def updateGrid(grid: Grid, stuck: Boolean): Grid = {
    val (r, c) = (grid.length - 1, grid(0).length - 1)
    val nextGrid = Array.ofDim[Boolean](r + 1, c + 1)

    val newGrid = haloPaddingGrid(grid)

    for (y <- 0 to r; x <- 0 to c) {
        nextGrid(y)(x) = lightCondition(newGrid, y + 1, x + 1)
    }

    if (stuck) {
        nextGrid(0)(0) = true
        nextGrid(0)(c) = true
        nextGrid(r)(0) = true
        nextGrid(r)(c) = true
    }

    return nextGrid.toList
}

def iterateGrid(grid: Grid, stuck: Boolean): Int = {
    val (r, c) = (grid.length - 1, grid(0).length - 1)
    var copyGrid = grid.map(_.clone())

    if (stuck) {
        copyGrid(0)(0) = true
        copyGrid(0)(c) = true
        copyGrid(r)(0) = true
        copyGrid(r)(c) = true
    }
    
    for (_ <- 1 to 100) { copyGrid = updateGrid(copyGrid, stuck) }

    return copyGrid.flatten.count(identity)
}

def evaluatorOne(grid: Grid): Int = iterateGrid(grid, false)
def evaluatorTwo(grid: Grid): Int = iterateGrid(grid, true)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day18.txt") match {
        case Success(lines) => {
            val grid = parseInput(lines)
            println(s"Part One: ${evaluatorOne(grid)}")
            println(s"Part Two: ${evaluatorTwo(grid)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}