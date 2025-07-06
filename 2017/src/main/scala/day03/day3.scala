package day03

import scala.collection.mutable.Map
import scala.util.boundary, boundary.break

case class Point(y: Int, x: Int) {
    def abs = x.abs + y.abs
}

val startPoint = Point(0, 0)

def getAllNeighbours(pos: Point): Seq[Point] = {
    return (for {
        dy <- -1 to 1
        dx <- -1 to 1
        if dy != 0 || dx != 0
    } yield Point(pos.y + dy, pos.x + dx))
}

def getCoordinates(n: Int): Point = {
    if (n == 1) return startPoint

    val ringNo = math.ceil((math.sqrt(n) - 1) / 2).toInt
    val start = (2 * ringNo - 1) * (2 * ringNo - 1)
    val offset = n - start - 1
    val side = offset / (2 * ringNo)
    val pos = offset % (2 * ringNo)

    return side match {
        case 0 => Point(-ringNo + (pos + 1), ringNo)
        case 1 => Point( ringNo, ringNo - (pos + 1))
        case 2 => Point( ringNo - (pos + 1), -ringNo)
        case _ => Point(-ringNo, -ringNo + (pos + 1))
    }
}

def firstSumAbove(input: Int): Int = {
    if (input == 1) return 1
    
    val grid = Map(startPoint -> 1)
    
    boundary {
        for (n <- Iterator.from(2)) {
            val pos = getCoordinates(n)
            val sum = getAllNeighbours(pos).map(pos => grid.getOrElse(pos, 0)).sum

            if (sum > input) break(sum)

            grid(pos) = sum
        }
    
        -1 // Unreachable
    }
}

def evaluatorOne(input: Int): Int = getCoordinates(input).abs
def evaluatorTwo(input: Int): Int = firstSumAbove(input)

def hello(): Unit = {
    val inputLine = 265149
    println(s"Part One: ${evaluatorOne(inputLine)}")
    println(s"Part Two: ${evaluatorTwo(inputLine)}")
}