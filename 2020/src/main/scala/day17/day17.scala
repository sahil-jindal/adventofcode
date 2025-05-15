package day17

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map, Set => MutableSet}

case class Vec3D(x: Int, y: Int, z: Int) {
    def +(that: Vec3D) = Vec3D(x + that.x, y + that.y, z + that.z)
}

case class Vec4D(x: Int, y: Int, z: Int, w: Int) {
    def +(that: Vec4D) = Vec4D(x + that.x, y + that.y, z + that.z, w + that.w)
}

def solve[T](input: List[String], create: (Int, Int) => T, neighbours: T => Seq[T]): Int = {
    var activePoints = (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
        if ch == '#'
    } yield create(x, y)).toSet

    for (_ <- 0 until 6) {
        val newActivePoints = MutableSet.empty[T]
        val inactivePoints = Map.empty[T, Int].withDefaultValue(0)

        for (point <- activePoints) {
            val activeNeighbours = neighbours(point).count(activePoints.contains)

            if (activeNeighbours == 2 || activeNeighbours == 3) {
                newActivePoints += point
            }

            for (neighbour <- neighbours(point) if !activePoints.contains(neighbour)) {
                inactivePoints(neighbour) += 1
            }
        }

        for ((point, activeNeighbours) <- inactivePoints if activeNeighbours == 3) {
            newActivePoints += point
        }

        activePoints = newActivePoints.toSet
    }

    return activePoints.size
}

def evaluatorOne(input: List[String]): Int = {
    val ds = (for {
        dx <- Seq(-1, 0, 1)
        dy <- Seq(-1, 0, 1)
        dz <- Seq(-1, 0, 1)
        if dx != 0 || dy != 0 || dz != 0
    } yield Vec3D(dx, dy, dz))

    return solve(input, (x, y) => Vec3D(x, y, 0), p => ds.map(_ + p))
}

def evaluatorTwo(input: List[String]): Int = {
    val ds = (for {
        dx <- Seq(-1, 0, 1)
        dy <- Seq(-1, 0, 1)
        dz <- Seq(-1, 0, 1)
        dw <- Seq(-1, 0, 1)
        if dx != 0 || dy != 0 || dz != 0 || dw != 0
    } yield Vec4D(dx, dy, dz, dw))

    return solve(input, (x, y) => Vec4D(x, y, 0, 0), p => ds.map(_ + p))
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day17.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}