package day12

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Set

case class Vec3D(x: Int, y: Int, z: Int) {
    def abs = x.abs + y.abs + z.abs
    def sign = Vec3D(x.sign, y.sign, z.sign)
    def +(that: Vec3D) = Vec3D(x + that.x, y + that.y, z + that.z)
    def -(that: Vec3D) = Vec3D(x - that.x, y - that.y, z - that.z)
}

case class Planet(var pos: Vec3D, var vel: Vec3D) {
    def move() = pos += vel
}

def step(planets: List[Planet]): List[Planet] = {
    for (planetA <- planets; planetB <- planets) {
        planetA.vel += (planetB.pos - planetA.pos).sign
    }

    planets.foreach(_.move())

    return planets
}

def simulate(input: List[String]): Iterator[List[Planet]] = {
    val planets = input.map(line => {
        val List(x, y, z) = raw"(-?\d+)".r.findAllIn(line).map(_.toInt).toList
        Planet(Vec3D(x, y, z), Vec3D(0, 0, 0))
    })

    return Iterator.iterate(planets)(step)
}

def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)
def lcm(a: Long, b: Long): Long = a * (b / gcd(a, b))

def evaluatorOne(input: List[String]): Int = {
    return simulate(input).drop(1000).next().map(planet => planet.pos.abs * planet.vel.abs).sum
}

def evaluatorTwo(input: List[String]): Long = {
    def findCycle(dimExtract: Planet => (Int, Int)): Long = {
        val states = Set.empty[List[Int]]
        return simulate(input).indexWhere { planets =>
            val state = planets.map(dimExtract).flatMap(List(_, _))
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