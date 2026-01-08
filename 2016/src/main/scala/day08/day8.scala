package day08

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

sealed trait Instruction
case class Rect(width: Int, height: Int) extends Instruction
case class RotateRow(row: Int, shift: Int) extends Instruction
case class RotateCol(col: Int, shift: Int) extends Instruction

type Grid = Array[Array[Char]]

val rectPattern = raw"rect (\d+)x(\d+)".r
val rotateRowPattern = raw"rotate row y=(\d+) by (\d+)".r
val rotateColumnPattern = raw"rotate column x=(\d+) by (\d+)".r

def parseInput(input: List[String]) = input.collect {
    case rectPattern(width, height) => Rect(width.toInt, height.toInt)
    case rotateRowPattern(row, shift) => RotateRow(row.toInt, shift.toInt)
    case rotateColumnPattern(col, shift) => RotateCol(col.toInt, shift.toInt)
}

def executeInstructions(instructions: List[Instruction]): Grid = {
    val (height, width) = (6, 50)
    val screen = Array.fill(height, width)(' ')    
    
    for (instruction <- instructions) {
        instruction match {
            case Rect(rectWidth, rectHeight) => {
                for (y <- 0 until rectHeight; x <- 0 until rectWidth) {
                    screen(y)(x) = '#'
                }
            }
            case RotateRow(row, shift) => {
                val newRow = Array.ofDim[Char](width)

                for (x <- 0 until width) {
                    newRow((x + shift) % width) = screen(row)(x)
                }
                
                screen(row) = newRow
            }
            case RotateCol(col, shift) => {
                val newCol = Array.ofDim[Char](height)
                
                for (y <- 0 until height) {
                    newCol((y + shift) % height) = screen(y)(col)
                }

                for (y <- 0 until height) {
                    screen(y)(col) = newCol(y)
                }
            }
        }
    }

    return screen
}

def evaluatorOne(input: Grid): Int = input.flatten.count(_ == '#')
def evaluatorTwo(input: Grid): String = input.map(_.mkString).mkString("\n")

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day08.txt") match {
        case Success(lines) => {
            val input = executeInstructions(parseInput(lines))
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two:\n${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}