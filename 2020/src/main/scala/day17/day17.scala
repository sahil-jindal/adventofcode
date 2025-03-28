package day17

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Point3D(x: Int, y: Int, z: Int)
case class Point4D(x: Int, y: Int, z: Int, w: Int)

def solve[T](lines: List[String], create: (Int, Int) => T, neighbours: T => Seq[T]): Int = {
    val (height, width) = (lines.length, lines.head.length)

    var activePoints = (for {
        x <- 0 until width
        y <- 0 until height
        if lines(y)(x) == '#'
    } yield create(x, y)).toSet

    for (_ <- 0 until 6) {
        var newActivePoints = Set.empty[T]
        var inactivePoints = Map.empty[T, Int].withDefaultValue(0)

        for (point <- activePoints) {
            val activeNeighbours = neighbours(point).count(activePoints.contains)

            if (activeNeighbours == 2 || activeNeighbours == 3) {
                newActivePoints += point
            }

            for (neighbour <- neighbours(point) if !activePoints.contains(neighbour)) {
                inactivePoints += neighbour -> (inactivePoints(neighbour) + 1)
            }
        }

        for ((point, activeNeighbours) <- inactivePoints if activeNeighbours == 3) {
            newActivePoints += point
        }

        activePoints = newActivePoints
    }

    return activePoints.size
}

def evaluatorOne(input: List[String]): Int = {
    val ds = for {
        dx <- Seq(-1, 0, 1)
        dy <- Seq(-1, 0, 1)
        dz <- Seq(-1, 0, 1)
        if dx != 0 || dy != 0 || dz != 0
    } yield (dx, dy, dz)

    return solve(input, (x, y) => Point3D(x, y, 0), p => ds.map { 
        case (dx, dy, dz) => Point3D(p.x + dx, p.y + dy, p.z + dz) 
    })
}

def evaluatorTwo(input: List[String]): Int = {
    val ds = for {
        dx <- Seq(-1, 0, 1)
        dy <- Seq(-1, 0, 1)
        dz <- Seq(-1, 0, 1)
        dw <- Seq(-1, 0, 1)
        if dx != 0 || dy != 0 || dz != 0 || dw != 0
    } yield (dx, dy, dz, dw)

    return solve(input, (x, y) => Point4D(x, y, 0, 0), p => ds.map { 
        case (dx, dy, dz, dw) => Point4D(p.x + dx, p.y + dy, p.z + dz, p.w + dw) 
    })
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