package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.boundary, boundary.break

case class Vec2D(y: Int, x: Int) {
    def *(num: Int) = Vec2D(num * y, num * x)
    def -(that: Vec2D) = Vec2D(y - that.y, x - that.x)
    def +(that: Vec2D) = Vec2D(y + that.y, x + that.x)
}

case class Pair(dir: Vec2D, gcd: Int)
case class Input(station: Vec2D, asteroidsByDir: Map[Vec2D, Set[Int]])

def gcd(a: Int, b: Int): Int = if b == 0 then a else gcd(b, a % b)

def parseInput(input: List[String]): Set[Vec2D] = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex 
        if ch == '#'
    } yield Vec2D(y, x)).toSet
}

def getDirection(dir: Vec2D): Pair = {
    val g = gcd(dir.y, dir.x).abs
    return Pair(Vec2D(dir.y / g, dir.x / g), g)
}

def groupByDirection(station: Vec2D, remaining: Set[Vec2D]): Input = {
    return Input(station, remaining.map(it => getDirection(it - station)).groupMap(_.dir)(_.gcd))
}

def selectStationPosition(asteroids: Set[Vec2D]): Input = {
    return asteroids.map(it => groupByDirection(it, asteroids - it)).maxBy(_.asteroidsByDir.size)
}

def angle(dir: Vec2D): Double = {
    val result = math.atan2(dir.x, -dir.y).toDegrees
    return if (result < 0) { result + 360 } else { result }
}

def findPosition(numbers: List[Int], position: Int): (Int, Int) = {
    val length = numbers.max
    var currPosition = position

    val freq = Array.ofDim[Int](length + 1)
    numbers.foreach { it => freq(it) += 1 }

    val ringSizes = freq.scanRight(0)(_ + _).slice(1, length + 1)

    boundary {
        for ((ringSize, i) <- ringSizes.zipWithIndex) {
            if (currPosition <= ringSize) break((currPosition - 1, i))
            else { currPosition -= ringSize }
        }

        (-1, -1)
    }
}

def evaluatorOne(input: Input): Int = input.asteroidsByDir.size

def evaluatorTwo(input: Input): Int = {
    val Input(station, asteroidsByDir) = input

    // Using the fact that the problem is based on finite world
    val dirSizes = asteroidsByDir.view.mapValues(_.size).toMap

    require(dirSizes.values.sum >= 200)

    val (dirIdx, gcdIdx) = findPosition(dirSizes.values.toList, 200)

    // Great Place to use quickselect here, or a built-in that gives n-th smallest element
    val dir = dirSizes.collect { case (d, s) if s > gcdIdx => d }.toVector.sortBy(angle).apply(dirIdx)
    val gcd = asteroidsByDir(dir).toVector.sorted.apply(gcdIdx)
    
    val Vec2D(y, x) = station + dir * gcd
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