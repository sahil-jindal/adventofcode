package daysix

import scala.util.{Try, Success, Failure}
import scala.io.Source
import scala.collection.mutable.Set

// Constants
val EMPTY_CELL = '.'
val OBSTACLE_CELL = '#'
val START_CELL = '^'

case class Position(val x: Int, val y: Int)

// Directions
enum Direction(val velocity: (Int, Int)):
    case Up extends Direction((-1, 0))
    case Right extends Direction((0, 1))
    case Down extends Direction((1, 0))
    case Left extends Direction((0, -1))

def nextDirection(direction: Direction): Direction = direction match
    case Direction.Up    => Direction.Right
    case Direction.Right => Direction.Down
    case Direction.Down  => Direction.Left
    case Direction.Left  => Direction.Up

def findPosition(matrix: Array[Array[Char]]): Option[Position] =
    matrix.zipWithIndex.flatMap { case (row, rowIndex) =>
        row.zipWithIndex.collect { case (c, colIndex) 
            if c == START_CELL => Position(rowIndex, colIndex) 
        }
    }.headOption

def step(
    matrix: Array[Array[Char]],
    visitedMatrix: Array[Array[Boolean]],
    currentDirection: Direction
    ): (Boolean, Direction) =
    
    var pos = findPosition(matrix).getOrElse(Position(0, 0))
    val currentVelocity = currentDirection.velocity

    var nextCell: Option[Char] = Some(matrix(pos.x)(pos.y))

    while true do
        val prevPos = pos
        pos = Position(pos.x + currentVelocity._1, pos.y + currentVelocity._2)

        nextCell = matrix.lift(pos.x).flatMap(_.lift(pos.y))

        if nextCell.contains(OBSTACLE_CELL) then
            return (false, nextDirection(currentDirection))
        if nextCell.isEmpty then
            return (true, nextDirection(currentDirection))

        visitedMatrix(pos.x)(pos.y) = true
        matrix(pos.x)(pos.y) = START_CELL
        matrix(prevPos.x)(prevPos.y) = EMPTY_CELL

    (true, currentDirection) // This line should never be reached

def getMovementsMatrix(_matrix: Array[Array[Char]]): Array[Array[Boolean]] =
    val matrix = _matrix.map(_.clone)
    val visitedMatrix = matrix.map(_.map(_ == START_CELL))

    var isOutOfBounds = false
    var currentDirection = Direction.Up

    while !isOutOfBounds do
        val (outOfBounds, newDirection) = step(matrix, visitedMatrix, currentDirection)
        isOutOfBounds = outOfBounds
        currentDirection = newDirection

    visitedMatrix

def getDistinctPositions(matrix: Array[Array[Boolean]]): Int =
    matrix.map(_.count(identity)).sum

def getSolution(matrix: Array[Array[Char]]): Int =
    val movementsMatrix = getMovementsMatrix(matrix)
    getDistinctPositions(movementsMatrix)

// def evalutorTwo(manuals: List[Manual]): Unit =
//     val previouslyInorderlyManuals = manuals.filterNot(isOrdered)
//     previouslyInorderlyManuals.foreach(sortManual)
    
//     val sum = previouslyInorderlyManuals.map { manual =>
//         manual.pageNumbers(manual.pageNumbers.length / 2)
//     }.sum
    
//     println(sum)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Try {
        val source = Source.fromFile(filePath)
        
        try { 
            source.getLines().toList
        } finally { 
            source.close()
        }
    }

@main
def hello(): Unit =
    readLinesFromFile("src/main/scala/daysix/file.txt") match
        case Success(lines) => {  
            val input = lines.map(_.toCharArray).toArray      
            val result = getSolution(input)
            println(s"Distinct positions visited: $result")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }