package day03

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map => MutableMap}

case class Direction(dy: Int, dx: Int)

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

case class Group(y: Int, x: Int, distance1: Int, distance2: Int)

def parseInput(input: List[String]) = input.map(line => {
    line.split(",").map(it => (it.head, it.tail.toInt)).toList
})

def trace(path: List[(Char, Int)]): Map[Point, Int] = {
    val res = MutableMap.empty[Point, Int]
    var current = Point(0, 0)
    var distance = 0

    for ((ch, amount) <- path) {
        val dir = ch match {
            case 'U' => Direction(-1, 0)
            case 'D' => Direction(1, 0)
            case 'L' => Direction(0, -1)
            case 'R' => Direction(0, 1)
            case _ => throw Exception() 
        }

        for (_ <- 0 until amount) {
            current += dir
            distance += 1

            if (!res.contains(current)) {
                res.put(current, distance)
            }
        }
    }

    return res.toMap
}

def solve(paths: List[String], distance: Group => Int): Int = {
    val List(path1, path2) = parseInput(paths)
    val trace1 = trace(path1)
    val trace2 = trace(path2)

    val commonKeys = trace1.keySet & trace2.keySet
    return commonKeys.map(it => distance(Group(it.y, it.x, trace1(it), trace2(it)))).min
}

def evaluatorOne(input: List[String]) = solve(input, it => it.x.abs + it.y.abs)
def evaluatorTwo(input: List[String]) = solve(input, it => it.distance1 + it.distance2)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day03.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}