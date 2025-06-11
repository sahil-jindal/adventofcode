package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Range(start: Int, end: Int) {
    def isEmpty: Boolean = start > end
    def length: Long = if isEmpty then 0 else end - start + 1

    def intersect(that: Range): Range = {
        return Range(math.max(start, that.start), math.min(end, that.end))
    } 
}

case class Region(x: Range, y: Range, z: Range) {
    def isEmpty: Boolean = x.isEmpty || y.isEmpty || z.isEmpty
    def volume: Long = x.length * y.length * z.length

    def intersect(that: Region): Region = {
        return Region(x.intersect(that.x), y.intersect(that.y), z.intersect(that.z))
    }
}

case class Cmd(turnOff: Boolean, region: Region)

def parseInput(input: List[String]) = input.map(line => {
    val Seq(sx, ex, sy, ey, sz, ez) = raw"(-?\d+)".r.findAllIn(line).map(_.toInt).toSeq
    Cmd(line.startsWith("off"), Region(Range(sx, ex), Range(sy, ey), Range(sz, ez)))
})

def activeCubesInRange(cmds: List[Cmd], range: Int): Long = {
    // Recursive approach
    // If we can determine the number of active cubes in subregions
    // we can compute the effect of the i-th cmd as well:
    def activeCubesAfterIcmd(icmd: Int, region: Region): Long = {
        if (region.isEmpty || icmd < 0) return 0
        
        val intersection = region.intersect(cmds(icmd).region)
        val activeInRegion = activeCubesAfterIcmd(icmd - 1, region)
        val activeInIntersection = activeCubesAfterIcmd(icmd - 1, intersection)
        val activeOutsideIntersection = activeInRegion - activeInIntersection

        // outside the intersection is unaffected, the rest is either on or off:  
        return activeOutsideIntersection + (if cmds(icmd).turnOff then 0 else intersection.volume)
    }

    val side = Range(-range, range)

    return activeCubesAfterIcmd(cmds.length - 1, Region(side, side, side))
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