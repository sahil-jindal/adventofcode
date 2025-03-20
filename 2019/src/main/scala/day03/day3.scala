package day03

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable

case class GroupOne(dir: Char, amount: Int)
case class GroupTwo(y: Int, x: Int, distance1: Int, distance2: Int)

def parseInput(lines: List[String]) = lines.map(line => {
    line.split(",").map { it => GroupOne(it.head, it.tail.toInt) }.toList
})

def trace(path: List[GroupOne]): Map[(Int, Int), Int] = {
    val res = mutable.Map.empty[(Int, Int), Int]
    var (y, x, distance) = (0, 0, 0)

    for (it <- path) {
        val (dy, dx) = it.dir match {
            case 'U' => (-1, 0)
            case 'D' => (1, 0)
            case 'R' => (0, -1)
            case 'L' => (0, 1)
            case _ => throw Exception() 
        }

        for (_ <- 0 until it.amount) {
            y += dy
            x += dx
            distance += 1

            if (!res.contains((y, x))) {
                res.put((y, x), distance)
            }
        }
    }

    return res.toMap
}

def solve(paths: List[String], distance: GroupTwo => Int): Int = {
    val List(path1, path2) = parseInput(paths)
    val trace1 = trace(path1)
    val trace2 = trace(path2)

    val commonKeys = trace1.keySet & trace2.keySet
    return commonKeys.map { it => distance(GroupTwo(it._1, it._2, trace1(it), trace2(it))) }.min
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