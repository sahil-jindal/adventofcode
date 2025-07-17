package day06

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map, Set}
import scala.collection.immutable.Range.Inclusive

case class Point(y: Int, x: Int) {
    def manhattan(other: Point): Int = (x - other.x).abs + (y - other.y).abs
}

case class Plane(xRange: Inclusive, yRange: Inclusive) {
    def onLeftOrRight(pos: Point) = (pos.x == xRange.start || pos.x == xRange.end) && yRange.contains(pos.y)
    def onTopOrBottom(pos: Point) = (pos.y == yRange.start || pos.y == yRange.end) && xRange.contains(pos.x)
    def grid = for {y <- yRange; x <- xRange} yield Point(y, x)
}

def parseInput(input: List[String]): (Plane, List[Point]) = {
    val points = input.map(line => {
        val Array(x, y) = line.split(", ").map(_.toInt)
        Point(y, x)
    })

    val (ys, xs) = (points.map(_.y), points.map(_.x))
    return (Plane(xs.min to xs.max, ys.min to ys.max), points)
}

def findClosest(point: Point, points: List[Point]): Option[Int] = {
    val distances = points.map(point.manhattan)
    val minDistance = distances.min
    
    val closestIndices = distances.zipWithIndex.collect {
        case (distance, i) if distance == minDistance => i
    }

    if (closestIndices.size != 1) return None
    return Some(closestIndices.head)
}

def evaluatorOne(plane: Plane, points: List[Point]): Int = {
    val closestPointCounts = Map.empty[Int, Int].withDefaultValue(0)
    val infinitePoints = Set.empty[Int]

    for (point <- plane.grid) {
        val found = findClosest(point, points)

        if (found.isDefined) {
            val index = found.get
            closestPointCounts(index) += 1
            if (plane.onLeftOrRight(point) || plane.onTopOrBottom(point)) {
                infinitePoints.add(index)
            }
        }
    }

    return closestPointCounts.view.filterKeys(idx => !infinitePoints.contains(idx)).values.max
}

def evaluatorTwo(plane: Plane, points: List[Point]): Int = { 
    return plane.grid.count(point => points.map(point.manhattan).sum < 10000)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day06.txt") match {
        case Success(lines) => {
            val (plane, points) = parseInput(lines)
            println(s"Part One: ${evaluatorOne(plane, points)}")
            println(s"Part Two: ${evaluatorTwo(plane, points)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}