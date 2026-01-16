package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{ListBuffer, Map => MutableMap}

case class Vec2D(y: Int, x: Int) {
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

def rotate(dirs: List[Vec2D]): Iterator[Vec2D] = {
    val ordered = dirs.sortBy(dir => -math.atan2(dir.x.toDouble, dir.y.toDouble))
    return Iterator.continually(ordered).flatten
}

def destroy(input: Input): Iterator[Vec2D] = {
    val Input(station, asteroidsByDir) = input

    val mutableMap = MutableMap.from(asteroidsByDir.view.mapValues(list => {
        ListBuffer.from(list.sortBy(p => (p.y - station.y).abs + (p.x - station.x).abs))
    }))

    return rotate(mutableMap.keySet.toList).flatMap { dir =>
        mutableMap.get(dir).flatMap { list =>
            if (list.nonEmpty) Some(list.remove(0)) else {
                mutableMap.remove(dir)
                None
            }
        }
    }
}

def evaluatorOne(input: Input): Int = input.asteroidsByDir.size

def evaluatorTwo(input: Input): Int = {
    val asteroid = destroy(input).drop(199).next()
    asteroid.x * 100 + asteroid.y
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