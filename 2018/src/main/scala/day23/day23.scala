package day13

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.PriorityQueue
import scala.math.BigInt

case class Point(x: Int, y: Int, z: Int) {
    def manhattanDistance(other: Point): Int = (x - other.x).abs + (y - other.y).abs + (z - other.z).abs
}

case class Drone(pos: Point, r: Int) {
    def intersects(box: Box): Boolean = {
        val dx = math.max(0, math.max(box.min.x - pos.x, pos.x - box.max.x))
        val dy = math.max(0, math.max(box.min.y - pos.y, pos.y - box.max.y))
        val dz = math.max(0, math.max(box.min.z - pos.z, pos.z - box.max.z))
        return dx + dy + dz <= r
    }
}

case class Box(min: Point, size: Point) {
    val max: Point = Point(min.x + size.x - 1, min.y + size.y - 1, min.z + size.z - 1)

    def corners: Seq[Point] = Seq(
        min, Point(max.x, min.y, min.z), Point(min.x, max.y, min.z), Point(max.x, max.y, min.z),
        Point(min.x, min.y, max.z), Point(max.x, min.y, max.z), Point(min.x, max.y, max.z), max
    )

    def sizeValue: BigInt = BigInt(size.x) * BigInt(size.y) * BigInt(size.z)

    def dist: Int = corners.map(pt => pt.x.abs + pt.y.abs + pt.z.abs).min

    def divide: Seq[Box] = {
        val (sx, sy, sz) = (size.x / 2, size.y / 2, size.z / 2)
        val (tx, ty, tz) = (size.x - sx, size.y - sy, size.z - sz)
        
        return Seq(
            Box(min, Point(sx, sy, sz)), 
            Box(Point(min.x + sx, min.y, min.z), Point(tx, sy, sz)),
            Box(Point(min.x, min.y + sy, min.z), Point(sx, ty, sz)), 
            Box(Point(min.x, min.y, min.z + sz), Point(sx, sy, tz)), 
            Box(Point(min.x + sx, min.y + sy, min.z), Point(tx, ty, sz)),
            Box(Point(min.x + sx, min.y, min.z + sz), Point(tx, sy, tz)),
            Box(Point(min.x, min.y + sy, min.z + sz), Point(sx, ty, tz)), 
            Box(Point(min.x + sx, min.y + sy, min.z + sz), Point(tx, ty, tz))
        ).filter(box => box.size.x > 0 && box.size.y > 0 && box.size.z > 0)
    }
}

def parseInput(input: List[String]): List[Drone] = input.map(line => {
    val nums = "([-]?\\d+)".r.findAllIn(line).map(_.toInt).toList
    Drone(Point(nums(0), nums(1), nums(2)), nums(3))
})

def evaluatorOne(drones: List[Drone]): Int = {
    val maxDrone = drones.maxBy(_.r)
    return drones.count(_.pos.manhattanDistance(maxDrone.pos) <= maxDrone.r)
}

def evaluatorTwo(drones: List[Drone]): Int = {
    val (xs, ys, zs) = (drones.map(_.pos.x), drones.map(_.pos.y), drones.map(_.pos.z))
    val (minX, minY, minZ) = (xs.min, ys.min, zs.min)
    val (maxX, maxY, maxZ) = (xs.max, ys.max, zs.max)

    val box = Box(Point(minX, minY, minZ), Point(maxX - minX + 1, maxY - minY + 1, maxZ - minZ + 1))

    val pq = PriorityQueue.empty(Ordering.by[(Box, List[Drone]), (Int, Int)] { 
        case (box, drones) => (drones.size, -box.dist) 
    })

    pq.enqueue((box, drones))

    while (pq.nonEmpty) {
        val (currBox, currDrones) = pq.dequeue()
        if (currBox.sizeValue == 1) return currBox.dist
      
        for (subBox <- currBox.divide) {
            pq.enqueue((subBox, currDrones.filter(_.intersects(subBox))))
        }
    }

    throw new Exception("No solution found")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day23.txt") match {
        case Success(lines) => {
            val drones = parseInput(lines)
            println(s"Part One: ${evaluatorOne(drones)}")
            println(s"Part Two: ${evaluatorTwo(drones)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}