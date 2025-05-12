package day09

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map, Set}

case class Graph(locations: Set[String], distances: Map[(String, String), Int])

def parseLines(input: List[String]): Graph = {
    val locations = Set.empty[String]
    val distances = Map.empty[(String, String), Int]

    for (line <- input) {
        val Array(pair, cost) = line.split(" = ")
        val Array(place_1, place_2) = pair.split(" to ")

        locations ++= Set(place_1, place_2)
        distances((place_1, place_2)) = cost.toInt
        distances((place_2, place_1)) = cost.toInt
    }

    return Graph(locations, distances)
}

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
            val allPossibleCost = allPossiblePathCost(parseLines(lines))
            println(s"Part One: ${allPossibleCost.min}")
            println(s"Part Two: ${allPossibleCost.max}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}