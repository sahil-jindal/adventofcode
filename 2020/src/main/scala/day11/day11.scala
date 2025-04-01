package day11

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.annotation.tailrec

val directions = Seq((-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1))

type Grid = Array[Array[Char]]

val Empty = 'L'
val Occupied = '#'
val Floor = '.'

def gridsEqual(grid1: Grid, grid2: Grid): Boolean = {
    return (grid1 zip grid2).forall { case (row1, row2) => row1.sameElements(row2) }
}

@tailrec
def evolveUntilStable(grid: Grid, ruleFunc: Grid => Grid): Grid = {
    val newGrid = ruleFunc(grid)
    if (gridsEqual(grid, newGrid)) return newGrid
    return evolveUntilStable(newGrid, ruleFunc)
}

def countAdjacentOccupied(grid: Grid, row: Int, col: Int): Int = {
    val rows = grid.length
    val cols = grid(0).length
    
    return directions.count { case (dr, dc) =>
        val newRow = row + dr
        val newCol = col + dc
        newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols && grid(newRow)(newCol) == Occupied
    }
}

def countVisibleOccupied(grid: Grid, row: Int, col: Int): Int = {
    val rows = grid.length
    val cols = grid(0).length
    
    return directions.count { case (dr, dc) =>
        var currentRow = row + dr
        var currentCol = col + dc
        var foundSeat = false
        var isOccupied = false
      
        while (!foundSeat && currentRow >= 0 && currentRow < rows && currentCol >= 0 && currentCol < cols) {
            grid(currentRow)(currentCol) match {
                case Occupied => foundSeat = true; isOccupied = true
                case Empty => foundSeat = true
                case Floor => currentRow += dr; currentCol += dc
            }
        } 
      
        isOccupied
    }
}

def helper(occupiedLimit: Int, countFunction: (Grid, Int, Int) => Int): Grid => Grid = {
    def applyRules(grid: Grid): Grid = {
        val rows = grid.length
        val cols = grid(0).length
        val result = Array.ofDim[Char](rows, cols)
    
        for (r <- 0 until rows; c <- 0 until cols) {
            val adjacentOccupied = countFunction(grid, r, c)
      
            result(r)(c) = grid(r)(c) match {
                case Empty if adjacentOccupied == 0 => Occupied
                case Occupied if adjacentOccupied >= occupiedLimit => Empty
                case seat => seat
            }
        }
    
        return result
    }

    return applyRules
}

def evaluatorOne(grid: Grid): Int = {
    val applyRulesPart1 = helper(4, countAdjacentOccupied)
    return evolveUntilStable(grid, applyRulesPart1).flatten.count(_ == Occupied)
}

def evaluatorTwo(grid: Grid): Int = {
    val applyRulesPart2 = helper(5, countVisibleOccupied)
    return evolveUntilStable(grid, applyRulesPart2).flatten.count(_ == Occupied)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day11.txt") match {
        case Success(lines) => {
            val grid = lines.map(_.toArray).toArray
            println(s"Part One: ${evaluatorOne(grid)}")
            println(s"Part Two: ${evaluatorTwo(grid)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}