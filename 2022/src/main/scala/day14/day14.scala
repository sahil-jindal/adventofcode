package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map
import scala.util.control.Breaks._
import scala.collection.immutable.Range.Inclusive

case class Direction(dy: Int, dx: Int)

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

sealed trait LineSegment { def formPoints(): IndexedSeq[Point] }

case class Vertical(x: Int, yRange: Inclusive) extends LineSegment {
    override def formPoints() = yRange.map(y => Point(y, x))
}

case class Horizontal(y: Int, xRange: Inclusive) extends LineSegment {
    override def formPoints() = xRange.map(x => Point(y, x))
}

case class Cave(input: List[LineSegment], hasFloor: Boolean) {
    private val grid = Map.empty[Point, Char]

    input.flatMap(_.formPoints()).foreach(it => grid(it) = '#')
    private val maxFloor = grid.keys.map(_.y).max

    private def simulateFallingSand(sand: Point): Point = {
        val movements = List(Direction(1, 0), Direction(1, -1), Direction(1, 1))
        var current = sand

        breakable {
            while (current.y < maxFloor + 1) {
                val found = movements.find(it => !grid.contains(current + it))
                if (found.isEmpty) break()
                current += found.get
            }
        }

        return current
    }

    def fillWithSand(sandSource: Point): Int = {
        breakable {
            while (true) {
                val location = simulateFallingSand(sandSource)
                if (grid.contains(location)) break()  // already has sand there
                if (!hasFloor && location.y == maxFloor + 1) break()  // flows into the void
                grid(location) = 'o'
            }
        }
        
        return grid.values.count(_ == 'o')
    }
}

def parseInput(input: List[String]) = input.flatMap(line => {
    val steps = line.split(" -> ").collect {
        case s"$x,$y" => Point(y.toInt, x.toInt)
    }

    (steps.init zip steps.tail).collect {
        case (Point(y1, x1), Point(y2, x2)) if x1 == x2 => {
            val (sy, ey) = (y1.min(y2), y1.max(y2))
            Vertical(x1, sy to ey)
        }
        case (Point(y1, x1), Point(y2, x2)) if y1 == y2 => {
            val (sx, ex) = (x1.min(x2), x1.max(x2))
            Horizontal(y1, sx to ex)
        }
    }
})

def evaluatorOne(input: List[LineSegment]): Int = new Cave(input, false).fillWithSand(Point(0, 500))
def evaluatorTwo(input: List[LineSegment]): Int = new Cave(input, true).fillWithSand(Point(0, 500))

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