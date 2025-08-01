package day23

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.parallel.CollectionConverters._

case class Edge(from: String, to: String)

type Graph = Map[String, List[String]]

def parseEdge(line: String) = {
    val Array(a, b) = line.split("-")
    List(Edge(a, b), Edge(b, a))
}

def parseInput(input: List[String]) = input.flatMap(parseEdge).groupMap(_.from)(_.to)

def grow(graph: Graph, components: Set[List[String]]): Set[List[String]] = {
    return (for {
        members <- components.par
        neighbour <- members.flatMap(m => graph(m)).distinct
        if !members.contains(neighbour)
        if members.forall(graph(neighbour).contains)
    } yield (members :+ neighbour).sorted).seq
}

def evaluatorOne(graph: Graph): Int = {
    var components = graph.keySet.map(it => List(it))
    for (_ <- 0 until 2) { components = grow(graph, components) }
    return components.count(_.exists(_.startsWith("t")))
}

def evaluatorTwo(graph: Graph): String = {
    var components = graph.keySet.map(it => List(it))
    while (components.size > 1) { components = grow(graph, components) }
    return components.head.mkString(",")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day23.txt") match {
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