package day12

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Set

case class Point(var x: Int, var y: Int, var z: Int) {
    def absValue: Int = x.abs + y.abs + z.abs
}

case class Planet(pos: Point, vel: Point)

def step(planets: List[Planet]): List[Planet] = {
    for (planetA <- planets; planetB <- planets) {
        planetA.vel.x += (planetB.pos.x - planetA.pos.x).sign
        planetA.vel.y += (planetB.pos.y - planetA.pos.y).sign
        planetA.vel.z += (planetB.pos.z - planetA.pos.z).sign
    }

    planets.foreach { planet =>
        planet.pos.x += planet.vel.x
        planet.pos.y += planet.vel.y
        planet.pos.z += planet.vel.z
    }

    return planets
}

def simulate(input: List[String]): LazyList[List[Planet]] = {
    val planets = input.map(line => {
        val List(x, y, z) = "-?\\d+".r.findAllIn(line).map(_.toInt).toList
        Planet(Point(x, y, z), Point(0, 0, 0))
    })

    return LazyList.iterate(planets)(step)
}

def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)
def lcm(a: Long, b: Long): Long = a * (b / gcd(a, b))

def evaluatorOne(input: List[String]): Int = {
    return simulate(input).drop(1000).head.map(planet => planet.pos.absValue * planet.vel.absValue).sum
}

def evaluatorTwo(input: List[String]): Long = {
    def findCycle(dimExtract: Planet => (Int, Int)): Long = {
        val states = Set.empty[List[Int]]
        return simulate(input).indexWhere { planets =>
            val state = planets.map(dimExtract).flatMap { case (posD, vecD) => List(posD, vecD) }
            !states.add(state)
        }
    }

    val statesByX = findCycle(p => (p.pos.x, p.vel.x))
    val statesByY = findCycle(p => (p.pos.y, p.vel.y))
    val statesByZ = findCycle(p => (p.pos.z, p.vel.z))

    return lcm(statesByX, lcm(statesByY, statesByZ))
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day12.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}