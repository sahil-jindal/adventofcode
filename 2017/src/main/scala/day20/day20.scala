package day20

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Vec3D(x: Int, y: Int, z: Int) {
    def len = x.abs + y.abs + z.abs
    def +(that: Vec3D) = Vec3D(x + that.x, y + that.y, z + that.z)
}

case class Particle(id: Int, pos: Vec3D, vel: Vec3D, acc: Vec3D) {
    def step(): Particle = {
        val newVel = vel + acc
        val newPos = pos + newVel
        return copy(pos = newPos, vel = newVel)
    }

    def collisionTime(particle: Particle): List[Int] = {
        return (for {
            tx <- collisionTimeOnAxis(particle.acc.x - acc.x, particle.vel.x - vel.x, particle.pos.x - pos.x)
            ty <- collisionTimeOnAxis(particle.acc.y - acc.y, particle.vel.y - vel.y, particle.pos.y - pos.y)
            tz <- collisionTimeOnAxis(particle.acc.z - acc.z, particle.vel.z - vel.z, particle.pos.z - pos.z)
            if tx == ty && ty == tz
        } yield tx)
    }

    def collisionTimeOnAxis(da: Int, dv: Int, dp: Int): List[Int] = {
        return solveIntEq(da / 2, dv, dp)
    }

    def solveIntEq(a: Int, b: Int, c: Int): List[Int] = {
        if (a == 0) {
            if (b != 0) return List(-c / b)
            if (c == 0) return List(0)
            return List.empty    
        }

        val d = b * b - 4 * a * c
            
        if (d < 0) return List.empty
        if (d == 0) return List(-b / (2 * a))
            
        val ds = math.sqrt(d)
            
        if (ds * ds != d) return List.empty
            
        return List(((-b + ds) / (2 * a)).toInt, ((-b - ds) / (2 * a)).toInt)
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
    var particles = currParticles

    val T = (for {
        p1 <- particles
        p2 <- particles 
        if p1.id != p2.id
        ct <- p1.collisionTime(p2)
    } yield ct).max

    for (_ <- 0 to T) {
        val positionFreq = particles.groupMapReduce(_.pos)(_ => 1)(_ + _)
        particles = particles.filterNot(it => positionFreq(it.pos) > 1).map(_.step())
    }
    
    return particles.length
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