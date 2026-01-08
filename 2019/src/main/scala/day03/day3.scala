package day03

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Direction(dy: Int, dx: Int)

case class Point(y: Int, x: Int) {
    def abs = x.abs + y.abs
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

case class Pair(pos: Point, distance: Int)

type WirePath = Map[Point, Int]

def parseLine(line: String) = line.split(",").collect {
    case s"R$num" => (Direction(0, 1)  -> num.toInt)
    case s"D$num" => (Direction(1, 0)  -> num.toInt)
    case s"U$num" => (Direction(-1, 0) -> num.toInt)
    case s"L$num" => (Direction(0, -1) -> num.toInt)
}.toList

def parseInput(input: List[String]) = input.map(parseLine)

def preComputeTrace(path: List[(Direction, Int)]): WirePath = {
    val directions = path.flatMap { case (dir, amount) => List.fill(amount)(dir) }
    return directions.scanLeft(Point(0, 0))(_ + _).zipWithIndex.tail.distinctBy(_._1).toMap
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