package day11

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map => MutableMap}

type Grid = Map[String, List[String]]

def parseInput(input: List[String]) = input.map(line => {
    val parts = raw"\w{3}".r.findAllIn(line).toList
    parts.head -> parts.tail
}).toMap

def pathCount(
    g: Grid, from: String, to: String, 
    cache: MutableMap[String, Long]
): Long = {
    return cache.getOrElseUpdate(from, {
        if from == to then 1 else {
            g.getOrElse(from, List.empty).map(next => {
                pathCount(g, next, to, cache)
            }).sum
        }
    })
}

def evaluatorOne(g: Grid) = pathCount(g, "you", "out", MutableMap.empty)

def evaluatorTwo(g: Grid): Long = {
    val a = pathCount(g, "svr", "fft", MutableMap.empty)
    val b = pathCount(g, "fft", "dac", MutableMap.empty)
    val c = pathCount(g, "dac", "out", MutableMap.empty)
    val d = pathCount(g, "svr", "dac", MutableMap.empty)
    val e = pathCount(g, "dac", "fft", MutableMap.empty)
    val f = pathCount(g, "fft", "out", MutableMap.empty)
    return a*b*c + d*e*f
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day11.txt") match {
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