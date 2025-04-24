package day18

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable

case class Point(x: Int, y: Int, z: Int)
case class Bounds(min: Point, max: Point)

def getLavaLocations(input: List[String]): List[Point] = input.map(line => {
    val coords = line.split(",").map(_.toInt)
    Point(coords(0), coords(1), coords(2))
})

def neighbours(point: Point) = List(
    point.copy(x = point.x - 1),
    point.copy(x = point.x + 1),
    point.copy(y = point.y - 1),
    point.copy(y = point.y + 1),
    point.copy(z = point.z - 1),
    point.copy(z = point.z + 1)
)

def getBounds(points: List[Point]): Bounds = {
    val xs = points.map(_.x)
    val ys = points.map(_.y)
    val zs = points.map(_.z)

    return Bounds(
        Point(xs.min - 1, ys.min - 1, zs.min - 1),
        Point(xs.max + 1, ys.max + 1, zs.max + 1)
    )
}

def within(bounds: Bounds, point: Point): Boolean = {
    bounds.min.x <= point.x && point.x <= bounds.max.x &&
    bounds.min.y <= point.y && point.y <= bounds.max.y &&
    bounds.min.z <= point.z && point.z <= bounds.max.z
}

def fillWithWater(from: Point, bounds: Bounds, lavaLocations: List[Point]): Set[Point] = {
    val result = mutable.Set(from)
    val pq = mutable.Queue(from)

    while (pq.nonEmpty) {
        val water = pq.dequeue()

        for (neighbour <- neighbours(water)) {
            if (!result.contains(neighbour) && 
                within(bounds, neighbour) && 
                !lavaLocations.contains(neighbour)
            ) {
                result += neighbour
                pq += neighbour
            }
        }
    }

    return result.toSet
}

def evaluatorOne(lavaLocations: List[Point]): Int = {
    return lavaLocations.flatMap(neighbours).count(p => !lavaLocations.contains(p))
}

def evaluatorTwo(lavaLocations: List[Point]): Int = {
    val bounds = getBounds(lavaLocations)
    val waterLocations = fillWithWater(bounds.min, bounds, lavaLocations)
    return lavaLocations.flatMap(neighbours).count(p => waterLocations.contains(p))
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day18.txt") match {
        case Success(lines) => {
            val input = getLavaLocations(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}