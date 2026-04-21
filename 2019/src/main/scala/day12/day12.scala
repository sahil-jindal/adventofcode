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

case class Planet(pos: Vec3D, vel: Vec3D) {
    def move(acc: Vec3D): Planet = {
        val newVel = vel + acc
        val newPos = pos + newVel
        return Planet(newPos, newVel)
    }
}

def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)
def lcm(a: Long, b: Long): Long = a * (b / gcd(a, b))

def parseInput(input: List[String]) = input.map(line => {
    val List(x, y, z) = raw"(-?\d+)".r.findAllIn(line).map(_.toInt).toList
    Planet(Vec3D(x, y, z), Vec3D(0, 0, 0))
})

def step(planets: List[Planet]) = planets.map(pA => {
    var acc = Vec3D(0, 0, 0) 
    
    for (pB <- planets; if pB != pA) {
        acc += (pB.pos - pA.pos).sign
    }

    pA.move(acc)
})

def simulate(planets: List[Planet]) = Iterator.iterate(planets)(step)

def evaluatorOne(input: List[Planet]): Int = {
    return simulate(input).drop(1000).next().map(it => it.pos.abs * it.vel.abs).sum
}

def evaluatorTwo(input: List[Planet]): Long = {
    def findCycle(dimExtract: Planet => (Int, Int)): Long = {
        val states = Set.empty[List[Int]]

        return simulate(input).indexWhere(planets => {
            val state = planets.map(dimExtract).flatMap(List(_, _))
            !states.add(state)
        })
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
            val input = parseInput(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}