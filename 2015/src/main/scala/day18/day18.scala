package day18

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Direction(dy: Int, dx: Int) {
    def +(that: Direction) = Direction(dy + that.dy, dx + that.dx)
}

case class Point(y: Int, x: Int)

type Grid = Array[Array[Boolean]]

val N = Direction(-1, 0)
val S = Direction(1, 0)
val E = Direction(0, 1)
val W = Direction(0, -1)
val NE = N + E
val NW = N + W
val SE = S + E
val SW = S + W

val topLeftCorner = List(S, SE, E) 
val topRightCorner = List(W, SW, S)  
val bottomLeftCorner = List(E, NE, N)
val bottomRightCorner = List(N, NW, W) 

val topBorder = List(W, SW, S, SE, E) 
val leftBorder = List(S, SE, E, NE, N)
val rightBorder = List(N, NW, W, SW, S) 
val bottomBorder = List(E, NE, N, NW, W) 

val insideBoxDyDx = List(NW, N, NE, E, SE, S, SW, W)

def parseInput(input: List[String]) = input.map(_.map(_ == '#').toArray).toArray

def lightCondition(grid: Grid, cell: Point, direction: List[Direction]): Boolean = {
    val valid = direction.count(dir => grid(cell.y + dir.dy)(cell.x + dir.dx))
    if grid(cell.y)(cell.x) then return valid == 2 || valid == 3
    return valid == 3
}

def updateGrid(grid: Grid, stuck: Boolean): Grid = {
    val (height, width) = (grid.length, grid(0).length)
    val nextGrid = Array.ofDim[Boolean](height, width)

    if stuck then {
        nextGrid(0)(0) = true
        nextGrid(0)(width - 1) = true
        nextGrid(height - 1)(0) = true
        nextGrid(height - 1)(width - 1) = true
    } else {
        nextGrid(0)(0) = lightCondition(grid, Point(0, 0), topLeftCorner)
        nextGrid(0)(width - 1) = lightCondition(grid, Point(0, width - 1), topRightCorner)
        nextGrid(height - 1)(0) = lightCondition(grid, Point(height - 1, 0), bottomLeftCorner)
        nextGrid(height - 1)(width - 1) = lightCondition(grid, Point(height - 1, width - 1), bottomRightCorner)
    }

    for (i <- 1 to width - 2) {
        nextGrid(0)(i) = lightCondition(grid, Point(0, i), topBorder)
        nextGrid(height - 1)(i) = lightCondition(grid, Point(height - 1, i), bottomBorder)
    }

    for (i <- 1 to height - 2) {
        nextGrid(i)(0) = lightCondition(grid, Point(i, 0), leftBorder)
        nextGrid(i)(width - 1) = lightCondition(grid, Point(i, width - 1), rightBorder)
    }

    for (i <- 1 to height - 2; j <- 1 to width - 2) {
        nextGrid(i)(j) = lightCondition(grid, Point(i, j), insideBoxDyDx)
    }

    return nextGrid
}

def iterateGrid(grid: Grid, stuck: Boolean): Int = {
    var copyGrid = grid.map(_.clone())

    val (height, width) = (grid.length, grid(0).length)

    if stuck then {
        copyGrid(0)(0) = true
        copyGrid(0)(width - 1) = true
        copyGrid(height - 1)(0) = true
        copyGrid(height - 1)(width - 1) = true
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