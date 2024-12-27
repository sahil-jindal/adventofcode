package daysix

import scala.util.{Try, Success, Failure}
import scala.io.Source
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

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

def checkLoop(matrix: Array[Array[Char]], obstacleX: Int, obstacleY: Int): Boolean =
    val clonedMatrix = matrix.map(_.clone)
    clonedMatrix(obstacleX)(obstacleY) = OBSTACLE_CELL
    val visitedMatrix = clonedMatrix.map(_.map(_ == START_CELL))

    val startPosition = findPosition(clonedMatrix).getOrElse(Position(0, 0))
    var currentDirection = Direction.Up

    val sequence = ArrayBuffer((startPosition, currentDirection))

    while true do
        val (outOfBounds, newDirection) = step(clonedMatrix, visitedMatrix, currentDirection)
        if outOfBounds then return false
        currentDirection = newDirection

        findPosition(clonedMatrix) match
            case Some(pos) =>
                if sequence.contains((pos, currentDirection)) then return true
                sequence.append((pos, currentDirection))
            case None => return false

def getCoordinates(matrix: Array[Array[Char]], visitedMatrix: Array[Array[Boolean]]): ArrayBuffer[Position] = 
    val coordinates = ArrayBuffer[Position]()
    
    for (i <- visitedMatrix.indices; j <- visitedMatrix(i).indices if visitedMatrix(i)(j)) {
        val startPos = findPosition(matrix)
        if (!startPos.contains(Position(i, j)))
            coordinates.append(Position(i, j))
    }
    
    coordinates

def evalutorOne(matrix: Array[Array[Char]]): Int =
    getMovementsMatrix(matrix).map(_.count(identity)).sum

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
            val result = evalutorOne(input)
            println(s"Distinct positions visited: $result")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }