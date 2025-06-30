package day06

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Instruction(action: String, startx: Int, starty: Int, endx: Int, endy: Int)

val instructionRegex = raw"(toggle|turn on|turn off) (\d+),(\d+) through (\d+),(\d+)".r

def parseInput(input: List[String]) = input.map(line => {
    val List(action, a, b, c, d) = instructionRegex.findFirstMatchIn(line).get.subgroups
    Instruction(action, a.toInt, b.toInt, c.toInt, d.toInt)
})

def evaluatorOne(input: List[Instruction]): Int = {
    
    def turnOnBrightness(grid: Array[Array[Boolean]], startx: Int, starty: Int, endx: Int, endy: Int) = {
        for { i <- starty to endy; j <- startx to endx } do grid(i)(j) = true
    }

    def turnOffBrightness(grid: Array[Array[Boolean]], startx: Int, starty: Int, endx: Int, endy: Int) = {
        for { i <- starty to endy; j <- startx to endx } do grid(i)(j) = false
    }

    def toggleBrightness(grid: Array[Array[Boolean]], startx: Int, starty: Int, endx: Int, endy: Int) = {
        for { i <- starty to endy; j <- startx to endx } do grid(i)(j) = !grid(i)(j)
    }
    
    val grid = Array.ofDim[Boolean](1000, 1000)

    for (Instruction(action, startx, starty, endx, endy) <- input) {
        action match {
            case "turn on" => turnOnBrightness(grid, startx, starty, endx, endy)
            case "turn off" => turnOffBrightness(grid, startx, starty, endx, endy)
            case "toggle" => toggleBrightness(grid, startx, starty, endx, endy)
        }
    }

    return grid.flatten.count(identity)
}

def evaluatorTwo(input: List[Instruction]): Int = {
    
    def turnOnBrightness(grid: Array[Array[Int]], startx: Int, starty: Int, endx: Int, endy: Int) = {
        for { i <- starty to endy; j <- startx to endx } do grid(i)(j) += 1
    }

    def turnOffBrightness(grid: Array[Array[Int]], startx: Int, starty: Int, endx: Int, endy: Int) = {
        for { i <- starty to endy; j <- startx to endx } do grid(i)(j) = math.max(0, grid(i)(j) - 1)
    }

    def toggleBrightness(grid: Array[Array[Int]], startx: Int, starty: Int, endx: Int, endy: Int) = {
        for { i <- starty to endy; j <- startx to endx } do grid(i)(j) += 2
    }
    
    val grid = Array.ofDim[Int](1000, 1000)

    for (Instruction(action, startx, starty, endx, endy) <- input) {
        action match {
            case "turn on" => turnOnBrightness(grid, startx, starty, endx, endy)
            case "turn off" => turnOffBrightness(grid, startx, starty, endx, endy)
            case "toggle" => toggleBrightness(grid, startx, starty, endx, endy)
        }
    }

    return grid.flatten.sum
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day06.txt") match {
        case Success(lines) => {
            val input = parseInput(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}