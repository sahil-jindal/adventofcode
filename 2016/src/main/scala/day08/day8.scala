package day08

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

val rectPattern = "rect (\\d+)x(\\d+)".r
val rotateRowPattern = "rotate row y=(\\d+) by (\\d+)".r
val rotateColumnPattern = "rotate column x=(\\d+) by (\\d+)".r

def executeInstructions(instructions: List[String]) = {
    val (height, width) = (6, 50)
    val screen = Array.fill(height, width)(false)    
    
    instructions.foreach { instruction =>
        instruction match {
            case rectPattern(a, b) => {
                val rectWidth = a.toInt
                val rectHeight = b.toInt
                
                for (y <- 0 until rectHeight; x <- 0 until rectWidth) {
                    screen(y)(x) = true
                }
            }
            case rotateRowPattern(a, b) => {
                val row = a.toInt
                val shift = b.toInt
                val newRow = Array.ofDim[Boolean](width)

                for (x <- 0 until width) {
                    newRow((x + shift) % width) = screen(row)(x)
                }
                
                screen(row) = newRow
            }
            case rotateColumnPattern(a, b) => {
                val col = a.toInt
                val shift = b.toInt
                val newCol = Array.ofDim[Boolean](height)
                
                for (y <- 0 until height) {
                    newCol((y + shift) % height) = screen(y)(col)
                }

                for (y <- 0 until height) {
                    screen(y)(col) = newCol(y)
                }
            }
            case _ => println(s"Unrecognized instruction: $instruction")
        }
    }

    screen
}

def evaluatorOne(instructions: List[String]): Int = {
    val screen = executeInstructions(instructions)
    screen.flatten.count(identity)
}

def evaluatorTwo(instructions: List[String]) = {
    val screen = executeInstructions(instructions)
    screen.map { row => row.map(if (_) '#' else '.').mkString }.mkString("\n")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("day08.txt") match
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two:\n${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }