package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Inclusive(start: Int, end: Int) {
    def isEmpty: Boolean = start > end
    def length: Long = if isEmpty then 0 else end - start + 1

    def intersect(that: Inclusive): Inclusive = {
        return Inclusive(start.max(that.start), end.min(that.end))
    } 
}

case class Region(x: Inclusive, y: Inclusive, z: Inclusive) {
    def isEmpty: Boolean = x.isEmpty || y.isEmpty || z.isEmpty
    def volume: Long = x.length * y.length * z.length

    def intersect(that: Region): Region = {
        return Region(x.intersect(that.x), y.intersect(that.y), z.intersect(that.z))
    }
}

type Cmd = (turnOff: Boolean, region: Region)

def parseInput(input: List[String]) = input.map(line => {
    val Seq(sx, ex, sy, ey, sz, ez) = raw"(-?\d+)".r.findAllIn(line).map(_.toInt).toSeq
    (line.startsWith("off"), Region(Inclusive(sx, ex), Inclusive(sy, ey), Inclusive(sz, ez)))
})

def activeCubesInRange(cmds: List[Cmd], range: Int): Long = {
    // Recursive approach
    // If we can determine the number of active cubes in subregions
    // we can compute the effect of the i-th cmd as well:
    def activeCubesAfterIcmd(commands: List[Cmd], region: Region): Long = {
        if (region.isEmpty || commands.isEmpty) return 0
        
        val (turnOff, otherRegion) = commands.last

        val intersection = region.intersect(otherRegion)
        val activeInRegion = activeCubesAfterIcmd(commands.init, region)
        val activeInIntersection = activeCubesAfterIcmd(commands.init, intersection)
        val activeOutsideIntersection = activeInRegion - activeInIntersection

        // outside the intersection is unaffected, the rest is either on or off:  
        return activeOutsideIntersection + (if turnOff then 0 else intersection.volume)
    }

    val side = Inclusive(-range, range)

    return activeCubesAfterIcmd(cmds, Region(side, side, side))
}

def evaluatorOne(cmds: List[Cmd]): Long = activeCubesInRange(cmds, 50)
def evaluatorTwo(cmds: List[Cmd]): Long = activeCubesInRange(cmds, Int.MaxValue)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day22.txt") match {
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