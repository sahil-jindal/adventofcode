package day25

import scala.util.{Try, Success, Failure, Using, Random}
import scala.io.Source
import scala.collection.mutable.Map
import scala.util.control.Breaks._

case class Group(size: Int, c1: Int, c2: Int)

class DisjointUnionSets(nodes: Set[String]) {
    private val parent = Map.from(nodes.map(it => it -> it))
    private val size = Map.from(nodes.map(_ -> 1))
    private var componentCount = nodes.size

    def getComponentCount = componentCount

    def find(u: String): String = {
        if (parent(u) != u) {
            parent(u) = find(parent(u)) // Path compression
        }

        parent(u)
    }

    def union(u: String, v: String): Unit = {
        val rootU = find(u)
        val rootV = find(v)

        if (rootU != rootV) {
            if (size(rootU) < size(rootV)) {
                parent(rootU) = rootV
                size(rootV) += size(rootU)
            } else {
                parent(rootV) = rootU
                size(rootU) += size(rootV)
            }
            
            componentCount -= 1
        }
    }
}

def parseEdge(line: String) = {
    val parts = raw"\w{3}".r.findAllIn(line).toList
    val (u, nodes) = (parts.head, parts.tail)
    nodes.map(v => if (u.compareTo(v) < 0) then (u, v) else (v, u))
}

def findCut(edges: List[(String, String)], r: Random): Group = {
    val allNodes = edges.flatMap(List(_, _)).toSet
    val shuffledEdges = r.shuffle(edges)
    val uf = new DisjointUnionSets(allNodes)

    breakable {
        for ((u, v) <- shuffledEdges) {
            if (uf.getComponentCount <= 2) break()
            uf.union(u, v)
        }
    }

    val cutSize = edges.count { case (u, v) => uf.find(u) != uf.find(v) }

    val List(c1, c2) = allNodes.groupMapReduce(uf.find)(_ => 1)(_ + _).values.toList

    return Group(cutSize, c1, c2)
}

def solver(input: List[String]): Int = {
    val edges = input.flatMap(parseEdge).distinct
    val r = new Random()

    var g = findCut(edges, r)

    while (g.size != 3) {
        g = findCut(edges, r)
    } 

    return g.c1 * g.c2
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day25.txt") match {
        case Success(lines) => println(s"Answer: ${solver(lines)}")
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}