package day24

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.boundary, boundary.break

case class Vec2(x0: BigDecimal, x1: BigDecimal)
case class Vec3(x0: BigDecimal, x1: BigDecimal, x2: BigDecimal)
case class Particle2(pos: Vec2, vel: Vec2)
case class Particle3(pos: Vec3, vel: Vec3)
case class Pair(item1: BigDecimal, item2: BigDecimal)

def parseNum(line: String): List[BigDecimal] = {
    return raw"(-?\d+)".r.findAllIn(line).map(BigDecimal(_)).toList
}

def parseInput(input: List[String]) = input.map(line => {
    val List(px, py, pz, vx, vy, vz) = parseNum(line)
    Particle3(Vec3(px, py, pz), Vec3(vx, vy, vz))
})

def project(ps: List[Particle3], proj: Vec3 => Pair) = ps.map(p => {
    Particle2(
        Vec2(proj(p.pos).item1, proj(p.pos).item2),
        Vec2(proj(p.vel).item1, proj(p.vel).item2)
    )
})

def intersection(p1: Particle2, p2: Particle2): Option[Vec2] = {
    // this would look way better if I had a matrix library at my disposal.
    val determinant = p1.vel.x0 * p2.vel.x1 - p1.vel.x1 * p2.vel.x0
    
    if (determinant == 0) return None //particles don't meet
    
    val b0 = p1.vel.x0 * p1.pos.x1 - p1.vel.x1 * p1.pos.x0
    val b1 = p2.vel.x0 * p2.pos.x1 - p2.vel.x1 * p2.pos.x0
    
    return Some(Vec2(
        (p2.vel.x0 * b0 - p1.vel.x0 * b1) / determinant,
        (p2.vel.x1 * b0 - p1.vel.x1 * b1) / determinant
    ))
}

def solve2D(particles: List[Particle2]): Vec2 = {
    // We try to guess the speed of our stone (a for loop), then supposing 
    // that it is the right velocity we create a new reference frame that 
    // moves with that speed. The stone doesn't move in this frame, it has 
    // some fixed unknown coordinates. Now transform each particle into 
    // this reference frame as well. Since the stone is not moving, if we 
    // properly guessed the speed, we find that each particle meets at the 
    // same point. This must be the stone's location.

    def hits(p: Particle2, pos: Vec2): Boolean = {
        val d = (pos.x0 - p.pos.x0) * p.vel.x1 - (pos.x1 - p.pos.x1) * p.vel.x0
        return d.abs < BigDecimal(0.0001)
    }

    def translateV(p: Particle2, vel: Vec2): Particle2 = {
        return Particle2(p.pos, Vec2(p.vel.x0 - vel.x0, p.vel.x1 - vel.x1))
    }

    val s = 500 //arbitrary limits for the brute force that worked for me.
    
    boundary {
        for (v1 <- -s until s; v2 <- -s until s) {
            val vel = Vec2(v1, v2)

            intersection(translateV(particles(0), vel), translateV(particles(1), vel)) match {
                case Some(stone) => if (particles.forall(p => hits(translateV(p, vel), stone))) break(stone)
                case None => // Skip if no intersection
            }
        }

        throw Exception("No solution found")
    }
}

def evaluatorOne(particlesInit: List[Particle3]): Int = {
    val particles = project(particlesInit, v => Pair(v.x0, v.x1))

    def inRange(d: BigDecimal) = 2E+14 <= d && d <= 4E+14

    def inFuture(p: Particle2, pos: Vec2) = (pos.x0 - p.pos.x0).sign == p.vel.x0.sign
    
    return particles.combinations(2).count(it => {
        val pos = intersection(it(0), it(1))
        
        pos.isDefined && 
        inRange(pos.get.x0) && 
        inRange(pos.get.x1) &&
        inFuture(it(0), pos.get) && 
        inFuture(it(1), pos.get)
    })
}

def evaluatorTwo(particlesInit: List[Particle3]): BigDecimal = {
    val stoneXY = solve2D(project(particlesInit, vec => Pair(vec.x0, vec.x1)))
    val stoneXZ = solve2D(project(particlesInit, vec => Pair(vec.x0, vec.x2)))
    return (stoneXY.x0 + stoneXY.x1 + stoneXZ.x1).setScale(0, BigDecimal.RoundingMode.HALF_UP)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day24.txt") match {
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