package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map
import scala.util.control.Breaks._

case class Vec2D(y: Int, x: Int) {
    def sign = Vec2D(y.sign, x.sign)
    def *(num: Int) = Vec2D(y * num, x * num)
    def +(that: Vec2D) = Vec2D(y + that.y, x + that.x)
    def -(that: Vec2D) = Vec2D(y - that.y, x - that.x)
}

case class Cave(input: List[Array[Vec2D]], hasFloor: Boolean) {
    private val grid = Map.empty[Vec2D, Char]

    input.flatMap(steps => (steps.init zip steps.tail)).foreach(fillWithRocks)
    private val maxFoor = grid.keys.map(_.y).max

    private def fillWithRocks(start: Vec2D, end: Vec2D): Unit = {
        val dispmnt = end - start
        val dir = dispmnt.sign
        val length = 1 + math.max(dispmnt.x.abs, dispmnt.y.abs)
        grid ++= List.tabulate(length)(start + dir * _).map(_ -> '#')
    }

    private def simulateFallingSand(sand: Vec2D): Vec2D = {
        val movements = List(Vec2D(1, 0), Vec2D(1, -1), Vec2D(1, 1))
        var current = sand

        breakable {
            while (current.y < maxFoor + 1) {
                val found = movements.find(it => !grid.contains(current + it))
                if (found.isEmpty) break()
                current += found.get
            }
        }

        return current
    }

    def fillWithSand(sandSource: Vec2D): Int = {
        breakable {
            while (true) {
                val location = simulateFallingSand(sandSource)
                if (grid.contains(location)) break()  // already has sand there
                if (!hasFloor && location.y == maxFoor + 1) break()  // flows into the void
                grid(location) = 'o'
            }
        }
        
        return grid.values.count(_ == 'o')
    }
}

def parseInput(input: List[String]): List[Array[Vec2D]] = {
    return input.map(_.split(" -> ").collect {
        case s"$x,$y" => Vec2D(y.toInt, x.toInt)
    })
}

def evaluatorOne(input: List[Array[Vec2D]]): Int = new Cave(input, false).fillWithSand(Vec2D(0, 500))
def evaluatorTwo(input: List[Array[Vec2D]]): Int = new Cave(input, true).fillWithSand(Vec2D(0, 500))

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day14.txt") match {
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