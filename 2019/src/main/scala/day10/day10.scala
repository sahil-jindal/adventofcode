package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map, ListBuffer}

case class Direction(dy: Int, dx: Int)
case class Point(y: Int, x: Int)

type AsteroidsByDir = Map[Direction, ListBuffer[Point]]

case class Pair(station: Point, asteroidsByDir: AsteroidsByDir)

def gcd(a: Int, b: Int): Int = if b == 0 then a else gcd(b, a % b)

def asteroids(input: List[String]): List[Point] = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex 
        if ch == '#'
    } yield Point(y, x))
}

def selectStationPosition(input: List[String]): Pair = {
    var res = Pair(Point(0, 0), Map.empty[Direction, ListBuffer[Point]])
    val asteroidsList = asteroids(input)

    for (station <- asteroidsList) {
        val asteroidsByDir = Map.empty[Direction, ListBuffer[Point]]
        
        for (asteroid <- asteroidsList if station != asteroid) {
            val yDir = asteroid.y - station.y
            val xDir = asteroid.x - station.x
            val g = gcd(yDir, xDir).abs
            val dir = Direction(yDir / g, xDir / g)
            asteroidsByDir.getOrElseUpdate(dir, ListBuffer.empty) += asteroid
        }
        
        if (asteroidsByDir.size > res._2.size) {
            res = Pair(station, asteroidsByDir)
        }
    }

    return res
}

def rotate(dirs: Set[Direction]): Iterator[Direction] = {
    val ordered = dirs.toList.sortBy(dir => -math.atan2(dir.dx.toDouble, dir.dy.toDouble))
    return Iterator.continually(ordered).flatten
}

def destroy(input: List[String]): Iterator[Point] = {
    val Pair(station, asteroidsByDir) = selectStationPosition(input)
    
    for ((dir, list) <- asteroidsByDir) {
        asteroidsByDir(dir) = list.sortBy(p => (p.y - station.y).abs + (p.x - station.x).abs)
    }

    return rotate(asteroidsByDir.keySet.toSet).flatMap { dir =>
        asteroidsByDir.get(dir).flatMap { list =>
            if (list.nonEmpty) Some(list.remove(0)) else {
                asteroidsByDir.remove(dir)
                None
            }
        }
    }
}

def evaluatorOne(input: List[String]): Int = selectStationPosition(input).asteroidsByDir.size

def evaluatorTwo(input: List[String]): Int = {
    val asteroid = destroy(input).drop(199).next()
    asteroid.x * 100 + asteroid.y
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day10.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}