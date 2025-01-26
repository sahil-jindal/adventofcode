package day06

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

val instructionRegex = raw"(toggle|turn on|turn off) (\d+),(\d+) through (\d+),(\d+)".r

def evaluatorOne(lines: List[String]) = {
    
    def turnOnBrightness(grid: Array[Array[Boolean]], startx: Int, starty: Int, endx: Int, endy: Int) =
        for i <- starty to endy; j <- startx to endx do grid(i)(j) = true

    def turnOffBrightness(grid: Array[Array[Boolean]], startx: Int, starty: Int, endx: Int, endy: Int) =
        for i <- starty to endy; j <- startx to endx do grid(i)(j) = false

    def toggleBrightness(grid: Array[Array[Boolean]], startx: Int, starty: Int, endx: Int, endy: Int) =
        for i <- starty to endy; j <- startx to endx do grid(i)(j) = !grid(i)(j)
    
    val grid = Array.ofDim[Boolean](1000, 1000)

    for line <- lines do line match {
        case instructionRegex(action, a, b, c, d) => {
            val startx = a.toInt
            val starty = b.toInt
            val endx = c.toInt
            val endy = d.toInt

            action match {
                case "turn on" => turnOnBrightness(grid, startx, starty, endx, endy)
                case "turn off" => turnOffBrightness(grid, startx, starty, endx, endy)
                case "toggle" => toggleBrightness(grid, startx, starty, endx, endy)
            }
        }
        case _ => println(s"Invalid line: $line")
    }

    grid.flatten.count(identity)
}

def evaluatorTwo(lines: List[String]) = {
    
    def turnOnBrightness(grid: Array[Array[Int]], startx: Int, starty: Int, endx: Int, endy: Int) =
        for i <- starty to endy; j <- startx to endx do grid(i)(j) += 1

    def turnOffBrightness(grid: Array[Array[Int]], startx: Int, starty: Int, endx: Int, endy: Int) =
        for i <- starty to endy; j <- startx to endx do grid(i)(j) = math.max(0, grid(i)(j) - 1)

    def toggleBrightness(grid: Array[Array[Int]], startx: Int, starty: Int, endx: Int, endy: Int) =
        for i <- starty to endy; j <- startx to endx do grid(i)(j) += 2
    
    val grid = Array.ofDim[Int](1000, 1000)

    for line <- lines do line match {
        case instructionRegex(action, a, b, c, d) => {
            val startx = a.toInt
            val starty = b.toInt
            val endx = c.toInt
            val endy = d.toInt

            action match {
                case "turn on" => turnOnBrightness(grid, startx, starty, endx, endy)
                case "turn off" => turnOffBrightness(grid, startx, starty, endx, endy)
                case "toggle" => toggleBrightness(grid, startx, starty, endx, endy)
            }
        }
        case _ => println(s"Invalid line: $line")
    }

    grid.flatten.sum
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("day06.txt") match
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }