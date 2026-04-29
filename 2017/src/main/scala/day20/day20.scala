package day20

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Vec3D(x: Int, y: Int, z: Int) {
    def len = x.abs + y.abs + z.abs
    def +(that: Vec3D) = Vec3D(x + that.x, y + that.y, z + that.z)
    def -(that: Vec3D) = Vec3D(x - that.x, y - that.y, z - that.z)
}

case class Particle(id: Int, pos: Vec3D, vel: Vec3D, acc: Vec3D) {
    def step(): Particle = {
        val newVel = vel + acc
        val newPos = pos + newVel
        return copy(pos = newPos, vel = newVel)
    }

    def collisionTime(that: Particle): Set[Int] = {
        val (dp, dv, da) = (that.pos - pos, that.vel - vel, that.acc - acc)

        val tx = collisionTimeOnAxis(da.x, dv.x, dp.x)
        val ty = collisionTimeOnAxis(da.y, dv.y, dp.y)
        val tz = collisionTimeOnAxis(da.z, dv.z, dp.z)
        
        return tx & ty & tz
    }

    private def collisionTimeOnAxis(da: Int, dv: Int, dp: Int): Set[Int] = {
        return solveIntEq(da, 2*dv + da, 2*dp)
    }

    private def solveIntEq(a: Int, b: Int, c: Int): Set[Int] = {
        if (a == 0) {
            if (b != 0) return Set(-c / b)
            if (c == 0) return Set(0)
            return Set.empty    
        }

        val d = b * b - 4 * a * c
            
        if (d < 0) return Set.empty
        if (d == 0) return Set(-b / (2 * a))
            
        val ds = math.sqrt(d).toInt
            
        if (ds * ds != d) return Set.empty
            
        return Set(((-b + ds) / (2 * a)).toInt, ((-b - ds) / (2 * a)).toInt)
    }
}

def parseInput(input: List[String]): List[Particle] = {
    return input.zipWithIndex.map { case (line, id) =>
        val Array(pVec, vVec, aVec) = line.split(", ").map(str => {
            val Seq(x, y, z) = raw"(-?\d+)".r.findAllIn(str).map(_.toInt).toSeq
            Vec3D(x, y, z)
        })

        Particle(id, pVec, vVec, aVec)
    }
}

def evaluatorOne(currParticles: List[Particle]): Int = currParticles.minBy(_.acc.len).id

def evaluatorTwo(currParticles: List[Particle]): Int = {
    var particles = currParticles.toSet

    val T = (for {
        p1 <- particles
        p2 <- particles - p1 
    } yield p1.collisionTime(p2)).reduce(_ | _).max

    for (_ <- 0 to T) {
        val positionFreq = particles.groupMapReduce(_.pos)(_ => 1)(_ + _)
        particles = particles.withFilter(it => positionFreq(it.pos) == 1).map(_.step())
    }
    
    return particles.size
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day20.txt") match {
        case Success(lines) => {
            val particles = parseInput(lines)
            println(s"Part One: ${evaluatorOne(particles)}")
            println(s"Part Two: ${evaluatorTwo(particles)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}