package day09

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Graph(distances: Map[(String, String), Int]) {
    val locations = distances.keySet.flatMap(Set(_, _))
}

def parseLine(line: String): List[((String, String), Int)] = {
    val Array(pair, cost) = line.split(" = ")
    val Array(posA, posB) = pair.split(" to ")
    return List(((posA, posB), cost.toInt), ((posB, posA), cost.toInt))
}

def parseInput(input: List[String]) = Graph(input.flatMap(parseLine).toMap)

def allPossiblePathCost(graph: Graph): List[Int] = {
    return graph.locations.toSeq.permutations.map { route => 
        (route.init zip route.tail).map(graph.distances).sum
    }.toList
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day09.txt") match {
        case Success(lines) => {
            val allPossibleCost = allPossiblePathCost(parseInput(lines))
            println(s"Part One: ${allPossibleCost.min}")
            println(s"Part Two: ${allPossibleCost.max}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}