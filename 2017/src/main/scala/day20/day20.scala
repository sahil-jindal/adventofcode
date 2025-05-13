package day20

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Vec3D(x: Int, y: Int, z: Int) {
    def len: Int = x.abs + y.abs + z.abs
    def +(that: Vec3D) = Vec3D(x + that.x, y + that.y, z + that.z)
}

case class Particle(id: Int, pos: Vec3D, vel: Vec3D, acc: Vec3D) {
    var destroyed: Boolean = false

    def step(): Particle = {
        val newVel = vel + acc
        val newPos = pos + newVel
        return copy(pos = newPos, vel = newVel)
    }

    def collisionTime(particle: Particle): Seq[Int] = {
        return (for {
            tx <- collisionTimeOnAxis(particle.acc.x - acc.x, particle.vel.x - vel.x, particle.pos.x - pos.x)
            ty <- collisionTimeOnAxis(particle.acc.y - acc.y, particle.vel.y - vel.y, particle.pos.y - pos.y)
            tz <- collisionTimeOnAxis(particle.acc.z - acc.z, particle.vel.z - vel.z, particle.pos.z - pos.z)
            if tx == ty && ty == tz
        } yield tx)
    }

    def collisionTimeOnAxis(da: Int, dv: Int, dp: Int): Seq[Int] = {
        return solveIntEq(da / 2, dv, dp)
    }

    def solveIntEq(a: Int, b: Int, c: Int): Seq[Int] = {
        if (a == 0) {
            if (b != 0) return Seq(-c / b)
            if (c == 0) return Seq(0)
            return Seq.empty    
        }

        val d = b * b - 4 * a * c
            
        if (d < 0) return Seq.empty
        if (d == 0) return Seq(-b / (2 * a))
            
        val ds = math.sqrt(d)
            
        if (ds * ds != d) return Seq.empty
            
        return Seq(((-b + ds) / (2 * a)).toInt, ((-b - ds) / (2 * a)).toInt)
    }
}

def parseVector(s: String): Vec3D = {
    val Array(x, y, z) = s.split(",").map(_.trim.toInt)
    return Vec3D(x, y, z)
}

def parseInput(input: List[String]): List[Particle] = {
    val pattern = raw"p=<([^>]+)>, v=<([^>]+)>, a=<([^>]+)>".r

    return input.zipWithIndex.map { case (line, id) =>
        val List(pVec, vVec, aVec) = pattern.findFirstMatchIn(line).get.subgroups.map(parseVector)
        Particle(id, pVec, vVec, aVec)
    }
}

def evaluatorOne(currParticles: List[Particle]): Int = currParticles.minBy(_.acc.len).id

def evaluatorTwo(currParticles: List[Particle]): Int = {
    var particles = currParticles

    val collisionTimes = (for {
        p1 <- particles
        p2 <- particles 
        if p1.id != p2.id
        ct <- p1.collisionTime(p2)
    } yield ct)

    val T = collisionTimes.max
    var t = 0

    while (t <= T) {
        val particlesByPos = particles.sortBy(p => (p.pos.x, p.pos.y, p.pos.z))
        var particlePrev = particlesByPos(0)

        for (i <- 1 until particlesByPos.length) {
            val particle = particlesByPos(i)
            if (particlePrev.pos.x == particle.pos.x &&
                particlePrev.pos.y == particle.pos.y &&
                particlePrev.pos.z == particle.pos.z)
            {
                particlePrev.destroyed = true
                particle.destroyed = true
            }
            
            particlePrev = particle
        }

        particles = particles.filterNot(_.destroyed).map(_.step())

        t += 1
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