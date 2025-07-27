package day03

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map => MutableMap}

case class Direction(dy: Int, dx: Int)

case class Point(y: Int, x: Int) {
    def abs = x.abs + y.abs
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

case class Pair(pos: Point, distance: Int)

type WirePath = Map[Point, Int]

def getDirections(dir: Char) = dir match {
    case 'U' => Direction(-1, 0)
    case 'R' => Direction(0, 1)
    case 'D' => Direction(1, 0)
    case 'L' => Direction(0, -1)
    case _ => throw new Exception()
}

def parseInput(input: List[String]) = input.map(line => {
    line.split(",").map(it => (getDirections(it.head), it.tail.toInt)).toList
})

def preComputeTrace(path: List[(Direction, Int)]): WirePath = {
    val directions = path.flatMap { case (dir, amount) => List.fill(amount)(dir) }
    val points = directions.scanLeft(Point(0, 0))(_ + _)
    
    val result = MutableMap.empty[Point, Int]

    for ((pos, dist) <- points.zipWithIndex.tail) {
        if (!result.contains(pos)) {
            result += pos -> dist
        }
    }
    
    return result.toMap
}

def solve(traces: List[WirePath], distance: Pair => Int): Int = {
    val List(trace1, trace2) = traces
    return (trace1.keySet & trace2.keySet).map(it => distance(Pair(it, trace1(it) + trace2(it)))).min
}

def evaluatorOne(input: List[WirePath]) = solve(input, it => it.pos.abs)
def evaluatorTwo(input: List[WirePath]) = solve(input, it => it.distance)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day03.txt") match {
        case Success(lines) => {
            val traces = parseInput(lines).map(preComputeTrace)
            println(s"Part One: ${evaluatorOne(traces)}")
            println(s"Part Two: ${evaluatorTwo(traces)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}