package day23

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.PriorityQueue
import scala.math.BigInt

case class Vec3D(x: Int, y: Int, z: Int) {
    def len = x.abs + y.abs + z.abs
    def manhattan(other: Vec3D): Int = (x - other.x).abs + (y - other.y).abs + (z - other.z).abs
}

case class Drone(pos: Vec3D, r: Int) {
    def intersects(box: Box): Boolean = {
        val dx = math.max(0, math.max(box.min.x - pos.x, pos.x - box.max.x))
        val dy = math.max(0, math.max(box.min.y - pos.y, pos.y - box.max.y))
        val dz = math.max(0, math.max(box.min.z - pos.z, pos.z - box.max.z))
        return dx + dy + dz <= r
    }
}

case class Box(min: Vec3D, size: Vec3D) {
    val max = Vec3D(min.x + size.x - 1, min.y + size.y - 1, min.z + size.z - 1)

    def corners = List(
        min, Vec3D(max.x, min.y, min.z), Vec3D(min.x, max.y, min.z), Vec3D(max.x, max.y, min.z),
        Vec3D(min.x, min.y, max.z), Vec3D(max.x, min.y, max.z), Vec3D(min.x, max.y, max.z), max
    )

    def sizeValue = BigInt(size.x) * BigInt(size.y) * BigInt(size.z)

    def dist: Int = corners.map(_.len).min

    def divide: List[Box] = {
        val (sx, sy, sz) = (size.x / 2, size.y / 2, size.z / 2)
        val (tx, ty, tz) = (size.x - sx, size.y - sy, size.z - sz)
        
        return List(
            Box(min, Vec3D(sx, sy, sz)), 
            Box(Vec3D(min.x + sx, min.y, min.z), Vec3D(tx, sy, sz)),
            Box(Vec3D(min.x, min.y + sy, min.z), Vec3D(sx, ty, sz)), 
            Box(Vec3D(min.x, min.y, min.z + sz), Vec3D(sx, sy, tz)), 
            Box(Vec3D(min.x + sx, min.y + sy, min.z), Vec3D(tx, ty, sz)),
            Box(Vec3D(min.x + sx, min.y, min.z + sz), Vec3D(tx, sy, tz)),
            Box(Vec3D(min.x, min.y + sy, min.z + sz), Vec3D(sx, ty, tz)), 
            Box(Vec3D(min.x + sx, min.y + sy, min.z + sz), Vec3D(tx, ty, tz))
        ).filter(box => box.size.x > 0 && box.size.y > 0 && box.size.z > 0)
    }
}

def parseInput(input: List[String]) = input.map(line => {
    val Seq(x, y, z, r) = raw"(-?\d+)".r.findAllIn(line).map(_.toInt).toSeq
    Drone(Vec3D(x, y, z), r)
})

def evaluatorOne(drones: List[Drone]): Int = {
    val Drone(maxPos, maxR) = drones.maxBy(_.r)
    return drones.count(_.pos.manhattan(maxPos) <= maxR)
}

def evaluatorTwo(drones: List[Drone]): Int = {
    val positions = drones.map(_.pos)
    val (xs, ys, zs) = (positions.map(_.x), positions.map(_.y), positions.map(_.z))
    val (minX, minY, minZ) = (xs.min, ys.min, zs.min)
    val (maxX, maxY, maxZ) = (xs.max, ys.max, zs.max)

    val box = Box(Vec3D(minX, minY, minZ), Vec3D(maxX - minX + 1, maxY - minY + 1, maxZ - minZ + 1))

    val pq = PriorityQueue((box, drones))(using Ordering.by[(Box, List[Drone]), (Int, Int)] { 
        case (box, drones) => (drones.size, -box.dist) 
    })

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