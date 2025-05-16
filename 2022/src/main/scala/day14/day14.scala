package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map
import scala.util.control.Breaks._

case class Direction(dy: Int, dx: Int)

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

class Cave(input: List[String], hasFloor: Boolean) {
    private val map = Map.empty[Point, Char]

    private val maxFoor: Int = {
        for(line <- input) {
            val steps = line.split(" -> ").map(step => {
                val Array(x, y) = step.split(",").map(_.toInt)
                Point(y, x)
            })

            for (i <- 1 until steps.length) {
                fillWithRocks(steps(i - 1), steps(i))
            }
        }
        
        map.keys.map(_.y).max
    }

    def fillWithRocks(from: Point, to: Point): Int = {
        val dir = Direction((to.y - from.y).sign, (to.x - from.x).sign)
        var pos = from
        var steps = 0

        while (pos != to + dir) {
            map(pos) = '#'
            pos += dir
            steps += 1
        }
        
        steps
    }

    def fillWithSand(sandSource: Point): Int = {
        breakable {
            while (true) {
                val location = simulateFallingSand(sandSource)

                // already has sand there
                if (map.contains(location)) {
                    break()
                }

                // flows into the void
                if (!hasFloor && location.y == maxFoor + 1) {
                    break()
                }

                map(location) = 'o'
            }
        }
        
        return map.values.count(_ == 'o')
    }

    def simulateFallingSand(sand: Point): Point = {
        val down = Direction(1, 0)
        val left = Direction(1, -1)
        val right = Direction(1, 1)

        var current = sand

        breakable {
            while (current.y < maxFoor + 1) {
                if (!map.contains(current + down)) {
                    current += down
                } else if (!map.contains(current + left)) {
                    current += left
                } else if (!map.contains(current + right)) {
                    current += right
                } else {
                    break()
                }
            }
        }

        current
    }
}

def evaluatorOne(input: List[String]): Int = new Cave(input, false).fillWithSand(Point(0, 500))
def evaluatorTwo(input: List[String]): Int = new Cave(input, true).fillWithSand(Point(0, 500))

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day14.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}