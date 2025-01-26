package day09

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

def parseLines(lines: List[String]) = {
    val distances = Map[(String, String), Int]()

    for line <- lines do {
        val Array(pair, cost) = line.split(" = ")
        val Array(place_1, place_2) = pair.split(" to ")
        distances((place_1, place_2)) = cost.toInt
        distances((place_2, place_1)) = cost.toInt
    }

    distances
}

def allPossiblePathCost(distances: Map[(String, String), Int]) = {
    val locations = distances.keys.flatMap { case (a, b) => Seq(a, b) }.toSet
    val routes = locations.toSeq.permutations
    
    routes.map { route =>
        route.sliding(2).map { 
            case Seq(a, b) => distances((a, b)) 
        }.sum
    }
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("day09.txt") match
        case Success(lines) => {
            val allPossibleCost = allPossiblePathCost(parseLines(lines))
            println(s"Part One: ${allPossibleCost.min}")
            println(s"Part Two: ${allPossibleCost.max}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }