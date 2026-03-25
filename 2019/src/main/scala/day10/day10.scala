package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{ListBuffer, Map => MutableMap}

case class Vec2D(y: Int, x: Int) {
    def +(that: Vec2D) = Vec2D(y + that.y, x + that.x)
    def -(that: Vec2D) = Vec2D(y - that.y, x - that.x)
}

case class Input(station: Vec2D, asteroidsByDir: Map[Vec2D, List[Vec2D]])

def gcd(a: Int, b: Int): Int = if b == 0 then a else gcd(b, a % b)

def parseInput(input: List[String]): List[Vec2D] = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex 
        if ch == '#'
    } yield Vec2D(y, x)).toList
}

def splitAsteroidsByFocus(asteroids: List[Vec2D]) = {
    asteroids.indices.map(idx => (asteroids(idx), asteroids.take(idx) ++ asteroids.drop(idx + 1)))
}

def getDirection(asteroid: Vec2D, station: Vec2D): Vec2D = {
    val Vec2D(yDir, xDir) = asteroid - station
    val g = gcd(yDir, xDir).abs
    return Vec2D(yDir / g, xDir / g)
}

def lineOfSightGroups(station: Vec2D, remaining: List[Vec2D]): Input = {
    return Input(station, remaining.groupBy(it => getDirection(it, station)))
}

def selectStationPosition(asteroids: List[Vec2D]): Input = {
    return splitAsteroidsByFocus(asteroids).map(lineOfSightGroups).maxBy(_.asteroidsByDir.size)
}

def quadrant(point: Vec2D): Int = {
    (point.x >= 0, point.y >= 0) match {
        case (true, false) => 0  // x >= 0, y < 0
        case (true, true) => 1   // x >= 0, y >= 0
        case (false, true) => 2  // x < 0, y >= 0
        case (false, false) => 3 // x < 0, y < 0
    }
}

def angle(a: Vec2D, b: Vec2D): Int = {
    // Cross product: positive if b is clockwise from a
    (b.x * a.y) - (b.y * a.x)
}

def distance(point: Vec2D): Int = {
    point.x * point.x + point.y * point.y
}

def evaluatorOne(input: Input): Int = input.asteroidsByDir.size

def evaluatorTwo(input: Input): Int = {
    val Input(station, asteroidsByDir) = input
     
    // Sort by clockwise order: quadrant, then angle, then distance
    implicit val clockwiseOrdering: Ordering[Vec2D] = new Ordering[Vec2D] {
        def compare(a: Vec2D, b: Vec2D): Int = {
            val qcmp = quadrant(a).compare(quadrant(b))
            if (qcmp != 0) return qcmp
            
            val acmp = angle(a, b)
            if (acmp != 0) return acmp
            
            return distance(a).compare(distance(b))
        }
    }
    
    val sorted = asteroidsByDir.values.flatten.map(_ - station).toList.sorted
    
    // Build groups with (position_in_group, group_number, array_index)
    val groups = ListBuffer.empty[(Int, Int, Int)]
    var first = 0
    var second = 0
    
    groups += ((first, second, 0))
    
    for (i <- 1 until sorted.length) {
        if (angle(sorted(i), sorted(i - 1)) > 0) {
            // Different angle/direction
            first = 0
            second += 1
        } else {
            // Same angle/direction, further away
            first += 1
        }

        groups += ((first, second, i))
    }
    
    // Sort groups by (position_in_group, group_number) to get destruction order
    // Map back to absolute positions
    val asteroid = station + sorted(groups.sorted.apply(199)._3)
    return asteroid.x * 100 + asteroid.y
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