package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Segment(from: Int, to: Int) {
    def isEmpty: Boolean = from > to
    def length: Long = if isEmpty then 0 else to - from + 1

    def intersect(that: Segment): Segment = {
        return Segment(math.max(from, that.from), math.min(to, that.to))
    } 
}

case class Region(x: Segment, y: Segment, z: Segment) {
    def isEmpty: Boolean = x.isEmpty || y.isEmpty || z.isEmpty
    def volume: Long = x.length * y.length * z.length

    def intersect(that: Region): Region = {
        return Region(x.intersect(that.x), y.intersect(that.y), z.intersect(that.z))
    }
}

case class Cmd(turnOff: Boolean, region: Region)

def parseInput(input: List[String]): List[Cmd] = input.map(line => {
    val turnOff = line.startsWith("off")
    val m = raw"-?\d+".r.findAllIn(line).map(_.toInt).toList
    Cmd(turnOff, Region(Segment(m(0), m(1)), new Segment(m(2), m(3)), new Segment(m(4), m(5))))
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

    val side = Segment(-range, range)

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