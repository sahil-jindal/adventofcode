package day12

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Pair(from: String, to: String)

def parseInput(input: List[String]): Map[String, List[String]] = {
    return input.flatMap(line => {
        val Array(caveA, caveB) = line.split("-")
        List(Pair(caveA, caveB), Pair(caveB, caveA))
    }).groupMap(_.from)(_.to)
}

def explore(input: List[String], partTwo: Boolean): Int = {
    val map = parseInput(input)

    def pathCount(currentCave: String, visitedCaves: Set[String], anySmallCaveWasVisitedTwice: Boolean): Int = {
        if (currentCave == "end") return 1

        var res = 0

        for (cave <- map(currentCave)) {
            val isBigCave = cave.toUpperCase() == cave
            val seen = visitedCaves.contains(cave)

            if (!seen || isBigCave) {
                res += pathCount(cave, visitedCaves.incl(cave), anySmallCaveWasVisitedTwice)
            } else if (partTwo && !isBigCave && cave != "start" && !anySmallCaveWasVisitedTwice) {
                res += pathCount(cave, visitedCaves, true)
            }
        }

        return res
    }

    return pathCount("start", Set("start"), false)
}

def evaluatorOne(input: List[String]): Int = explore(input, false)
def evaluatorTwo(input: List[String]): Int = explore(input, true)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day12.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}