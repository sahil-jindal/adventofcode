package day23

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.parallel.CollectionConverters._

case class Edge(from: String, to: String)

type Graph = Map[String, List[String]]

def parseInput(input: List[String]): Graph = {
    val edges = input.flatMap(line => {
        val Array(a, b) = line.split("-")
        Seq(Edge(a, b), Edge(b, a))
    })

    return edges.groupMap(_.from)(_.to)
}

def Members(c: String) = c.split(",").toList

def Extend(c: String, item: String) = (Members(c) :+ item).sorted.mkString(",")

def grow(g: Graph, components: Set[String]): Set[String] = {
    return (for {
        c <- components.par
        members = Members(c)
        neighbour <- members.flatMap(m => g(m)).distinct
        if !members.contains(neighbour)
        if members.forall(m => g(neighbour).contains(m))
    } yield Extend(c, neighbour)).seq
}

def evaluatorOne(g: Graph): Int = {
    var components = g.keySet.toSet
    components = grow(g, components)
    components = grow(g, components)
    return components.map(Members).count(_.exists(_.startsWith("t")))
}

def evaluatorTwo(g: Graph): String = {
    var components = g.keySet.toSet

    while (components.size > 1) {
        components = grow(g, components)
    }

    return components.head
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