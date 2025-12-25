package day12

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Edge(from: String, to: String)

type Graph = Map[String, List[String]]

def parseInput(input: List[String]): Graph = {
    return input.flatMap(line => {
        val Array(caveA, caveB) = line.split("-")
        List(Edge(caveA, caveB), Edge(caveB, caveA))
    }).groupMap(_.from)(_.to)
}

def explore(graph: Graph, partTwo: Boolean): Int = {
    def pathCount(currentCave: String, visitedCaves: Set[String], anySmallCaveWasVisitedTwice: Boolean): Int = {
        if (currentCave == "end") return 1

        var res = 0

        for (cave <- graph(currentCave)) {
            val isBigCave = cave.toUpperCase() == cave
            val seen = visitedCaves.contains(cave)

            if (!seen || isBigCave) {
                res += pathCount(cave, visitedCaves + cave, anySmallCaveWasVisitedTwice)
            } else if (partTwo && !isBigCave && cave != "start" && !anySmallCaveWasVisitedTwice) {
                res += pathCount(cave, visitedCaves, true)
            }
        }

        return res
    }

    return pathCount("start", Set("start"), false)
}

def evaluatorOne(input: Graph): Int = explore(input, false)
def evaluatorTwo(input: Graph): Int = explore(input, true)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day12.txt") match {
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