package day11

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.annotation.tailrec

val directions = List((-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1))

enum Seat { case Empty, Occupied, Floor }

type Grid = Array[Array[Seat]]

def parseSeat(ch: Char) = ch match {
    case 'L' => Seat.Empty
    case '#' => Seat.Occupied
    case '.' => Seat.Floor
    case _ => throw new Exception()
}

def parseInput(input: List[String]) = input.map(_.map(parseSeat).toArray).toArray

def helper(occupiedLimit: Int, countFunction: (Grid, Int, Int) => Int): Grid => Grid = {
    return grid => {
        val (rows, cols) = (grid.length, grid(0).length)
        val result = Array.ofDim[Seat](rows, cols)
    
        for (r <- 0 until rows; c <- 0 until cols) {
            val adjacentOccupied = countFunction(grid, r, c)
      
            result(r)(c) = grid(r)(c) match {
                case Seat.Empty if adjacentOccupied == 0 => Seat.Occupied
                case Seat.Occupied if adjacentOccupied >= occupiedLimit => Seat.Empty
                case seat => seat
            }
        }
    
        result
    }
}

def countAdjacentOccupied(grid: Grid, row: Int, col: Int): Int = {
    val (rows, cols) = (grid.length, grid(0).length)
    
    return directions.count { case (dr, dc) =>
        val newRow = row + dr
        val newCol = col + dc
        newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols && grid(newRow)(newCol) == Seat.Occupied
    }
}

def countVisibleOccupied(grid: Grid, row: Int, col: Int): Int = {
    val (rows, cols) = (grid.length, grid(0).length)
    
    return directions.count { case (dr, dc) =>
        var currentRow = row + dr
        var currentCol = col + dc
        var foundSeat = false
        var isOccupied = false
      
        while (!foundSeat && currentRow >= 0 && currentRow < rows && currentCol >= 0 && currentCol < cols) {
            grid(currentRow)(currentCol) match {
                case Seat.Occupied => foundSeat = true; isOccupied = true
                case Seat.Empty => foundSeat = true
                case Seat.Floor => currentRow += dr; currentCol += dc
            }
        } 
      
        isOccupied
    }
}

def gridsEqual(grid1: Grid, grid2: Grid): Boolean = {
    return (grid1 zip grid2).forall { case (row1, row2) => row1.sameElements(row2) }
}

@tailrec
def evolveUntilStable(grid: Grid, ruleFunc: Grid => Grid): Grid = {
    val newGrid = ruleFunc(grid)
    if (gridsEqual(grid, newGrid)) return newGrid
    return evolveUntilStable(newGrid, ruleFunc)
}

def evaluatorOne(grid: Grid): Int = {
    val applyRulesPartOne = helper(4, countAdjacentOccupied)
    return evolveUntilStable(grid, applyRulesPartOne).flatten.count(_ == Seat.Occupied)
}

def evaluatorTwo(grid: Grid): Int = {
    val applyRulesPartTwo = helper(5, countVisibleOccupied)
    return evolveUntilStable(grid, applyRulesPartTwo).flatten.count(_ == Seat.Occupied)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day11.txt") match {
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