package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, Queue}

case class Vec2D(y: Int, x: Int) {
    def -(that: Vec2D) = Vec2D(y - that.y, x - that.x)
}

case class Input(station: Vec2D, asteroidsByDir: Map[Vec2D, Set[Vec2D]])

def gcd(a: Int, b: Int): Int = if b == 0 then a else gcd(b, a % b)

def parseInput(input: List[String]): Set[Vec2D] = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex 
        if ch == '#'
    } yield Vec2D(y, x)).toSet
}

def getDirection(dir: Vec2D): Vec2D = {
    val g = gcd(dir.y, dir.x).abs
    return Vec2D(dir.y / g, dir.x / g)
}

def groupByDirection(station: Vec2D, remaining: Set[Vec2D]): Input = {
    return Input(station, remaining.groupBy(it => getDirection(it - station)))
}

def selectStationPosition(asteroids: Set[Vec2D]): Input = {
    return asteroids.map(it => groupByDirection(it, asteroids - it)).maxBy(_.asteroidsByDir.size)
}

def angle(dir: Vec2D): Double = {
    val result = math.atan2(dir.x, -dir.y).toDegrees
    return if (result < 0) { result + 360 } else { result }
}

def distanceSq(point: Vec2D): Int = {
    val Vec2D(y, x) = point
    return x*x + y*y
}

def evaluatorOne(input: Input): Int = input.asteroidsByDir.size

def evaluatorTwo(input: Input): Int = {
    val Input(station, asteroidsByDir) = input

    val orderedDirection = asteroidsByDir.keySet.toList.sortBy(angle)
    
    val asteroids = ArrayBuffer.from(orderedDirection.map(dir => {
        Queue.from(asteroidsByDir(dir).toList.sortBy(it => distanceSq(it - station)))
    }))
     
    val vaporized = ArrayBuffer.empty[Vec2D]

    while (asteroids.exists(_.nonEmpty)) {
        var i = 0

        while (i < asteroids.length) {
            vaporized += asteroids(i).dequeue()

            if (asteroids(i).isEmpty) {
                asteroids.remove(i).clear()
            } else {
                i += 1
            }
        }
    }
    
    val Vec2D(y, x) = vaporized(199)
    return x * 100 + y
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day10.txt") match {
        case Success(lines) => {
            val input = selectStationPosition(parseInput(lines))
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}