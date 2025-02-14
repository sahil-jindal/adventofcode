package day20

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.math.{abs, sqrt}

class Point(var x: Int, var y: Int, var z: Int) {
    def len: Int = abs(x) + abs(y) + abs(z)
}

class Particle(val i: Int, val pos: Point, val vel: Point, val acc: Point) {
    var destroyed: Boolean = false

    def step(): Unit = {
        vel.x = vel.x + acc.x
        vel.y = vel.y + acc.y
        vel.z = vel.z + acc.z
        pos.x = pos.x + vel.x
        pos.y = pos.y + vel.y
        pos.z = pos.z + vel.z
    }

    def collisionTime(particle: Particle): Iterable[Int] = {
        for {
            tx <- collisionTimeOnAxis(particle.acc.x - acc.x, particle.vel.x - vel.x, particle.pos.x - pos.x)
            ty <- collisionTimeOnAxis(particle.acc.y - acc.y, particle.vel.y - vel.y, particle.pos.y - pos.y)
            tz <- collisionTimeOnAxis(particle.acc.z - acc.x, particle.vel.z - vel.z, particle.pos.z - pos.z)
            if tx == ty && ty == tz
        } yield tx
    }

    def collisionTimeOnAxis(da: Int, dv: Int, dp: Int): Iterable[Int] = {
        return solveIntEq(da / 2, dv, dp)
    }

    def solveIntEq(a: Int, b: Int, c: Int): Iterable[Int] = {
        if (a == 0) {
            if (b != 0) return Seq(-c / b)
            if (c == 0) return Seq(0)
            return Seq.empty    
        }

        val d = b * b - 4 * a * c
            
        if (d < 0) return Seq.empty
        if (d == 0) return Seq(-b / (2 * a))
            
        val ds = sqrt(d)
            
        if (ds * ds != d) return Seq.empty
            
        return Seq(((-b + ds) / (2 * a)).toInt, ((-b - ds) / (2 * a)).toInt)
    }
}

def parseVector(s: String): Point = {
    val parts = s.split(",").map(_.trim.toInt)
    return Point(parts(0), parts(1), parts(2))
}

def parseInput(lines: List[String]): List[Particle] = {
    val pattern = """p=<([^>]+)>, v=<([^>]+)>, a=<([^>]+)>""".r

    return lines.zipWithIndex.map { case (line, i) =>
        line match {
            case pattern(pStr, vStr, aStr) =>
                val pVec = parseVector(pStr)
                val vVec = parseVector(vStr)
                val aVec = parseVector(aStr)
                Particle(i, pVec, vVec, aVec)
            case _ =>
                sys.error(s"Line did not match expected format: $line")
        }
    }
}

def evaluatorOne(currParticles: List[Particle]): Int = currParticles.minBy(_.acc.len).i

def evaluatorTwo(currParticles: List[Particle]): Int = {
    var particles = currParticles

    val collisionTimes: Array[Int] = (for {
        p1 <- particles
        p2 <- particles if p1.i != p2.i
        ct <- p1.collisionTime(p2)
    } yield ct).toArray

    val T = collisionTimes.max
    var t = 0

    while (t <= T) {
        val particlesByPos = particles.sortBy(p => (p.pos.x, p.pos.y, p.pos.z)).toArray
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

        particles = particles.filterNot(_.destroyed)
        particles.foreach(_.step())

        t += 1
    }
    
    return particles.length
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Any =
    readLinesFromFile("day20.txt") match
        case Success(lines) => {
            val particles = parseInput(lines)
            println(s"Part One: ${evaluatorOne(particles)}")
            println(s"Part Two: ${evaluatorTwo(particles)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }