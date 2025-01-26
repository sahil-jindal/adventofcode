package day18

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Position(val y: Int, val x: Int)

val cornersDyDx = List(
    List((0, 1), (1, 0), (1, 1)),     // Top left corner
    List((0, -1), (1, -1), (1, 0)),   // Top right corner 
    List((-1, -1), (-1, 0), (0, -1)), // Bottom right corner
    List((-1, 0), (-1, 1), (0, 1))    // Bottom left corner
)

val bordersDyDx = List(
    List((0, -1), (0, 1), (1, -1), (1, 0), (1, 1)),    // Top Border
    List((-1, -1), (-1, 0), (0, -1), (1, -1), (1, 0)), // Right Border
    List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1)), // Bottom Border
    List((-1, 0), (-1, 1), (0, 1), (1, 0), (1, 1))     // Left Border
)

val insideBoxDyDx = List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))

def parseInput(line: String) = line.map { it => if it == '#' then 1 else 0 }.toArray

def lightCondition(grid: Array[Array[Int]], cell: Position, direction: List[(Int, Int)]) = {
    val valid = direction.count { case (dy, dx) => grid(cell.y + dy)(cell.x + dx) == 1 }

    if grid(cell.y)(cell.x) == 1 then {
        if valid == 2 || valid == 3 then 1 else 0
    } else {
        if valid == 3 then 1 else 0
    }
}

def updateGrid(grid: Array[Array[Int]], stuck: Boolean) = {
    val rowSize = grid.length
    val colSize = grid(0).length
    val nextGrid = Array.ofDim[Int](rowSize, colSize)

    if stuck then {
        nextGrid(0)(0) = 1
        nextGrid(0)(colSize - 1) = 1
        nextGrid(rowSize - 1)(colSize - 1) = 1
        nextGrid(rowSize - 1)(0) = 1
    } else {
        nextGrid(0)(0) = lightCondition(grid, Position(0, 0), cornersDyDx(0))
        nextGrid(0)(colSize - 1) = lightCondition(grid, Position(0, colSize - 1), cornersDyDx(1))
        nextGrid(rowSize - 1)(colSize - 1) = lightCondition(grid, Position(rowSize - 1, colSize - 1), cornersDyDx(2))
        nextGrid(rowSize - 1)(0) = lightCondition(grid, Position(rowSize - 1, 0), cornersDyDx(3))
    }

    for i <- 1 to colSize - 2 do {
        nextGrid(0)(i) = lightCondition(grid, Position(0, i), bordersDyDx(0))
        nextGrid(rowSize - 1)(i) = lightCondition(grid, Position(rowSize - 1, i), bordersDyDx(2))
    }

    for i <- 1 to rowSize - 2 do {
        nextGrid(i)(colSize - 1) = lightCondition(grid, Position(i, colSize - 1), bordersDyDx(1))
        nextGrid(i)(0) = lightCondition(grid, Position(i, 0), bordersDyDx(3))
    }

    for i <- 1 to rowSize - 2 do {
        for j <- 1 to colSize - 2 do {
            nextGrid(i)(j) = lightCondition(grid, Position(i, j), insideBoxDyDx)
        }
    }

    nextGrid
}

def evaluator(grid: Array[Array[Int]], stuck: Boolean) = {
    var copyGrid = grid

    val rowSize = grid.length
    val colSize = grid(0).length

    if stuck then {
        copyGrid(0)(0) = 1
        copyGrid(0)(colSize - 1) = 1
        copyGrid(rowSize - 1)(colSize - 1) = 1
        copyGrid(rowSize - 1)(0) = 1
    }
    
    for _ <- 1 to 100 do {
        copyGrid = updateGrid(copyGrid, stuck)
    }

    copyGrid.map(_.sum).sum
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("day18.txt") match
        case Success(lines) => {
            val grid = lines.map(parseInput).toArray
            println(s"Part One: ${evaluator(grid, false)}")
            println(s"Part Two: ${evaluator(grid, true)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }