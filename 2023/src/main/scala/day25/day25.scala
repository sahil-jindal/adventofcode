package day25

import scala.util.{Try, Success, Failure, Using, Random}
import scala.io.Source
import scala.collection.mutable.{Map, Set}

case class Group(size: Int, c1: Int, c2: Int)

def parseEdges(input: List[String]): List[(String, String)] = {
    val edgeSet = Set.empty[(String, String)]

    for (line <- input) {
        val parts = line.split(": ")
        val u = parts(0)
        val nodes = parts(1).split(" ")

        for (v <- nodes) {
            val (a, b) = if (u < v) (u, v) else (v, u)
            edgeSet.add((a, b))
        }
    }

    edgeSet.toList
}

class UnionFind(nodes: Set[String]) {
    private val parent = Map.empty[String, String]
    private val size = Map.empty[String, Int]
    private var componentCount = nodes.size

    for (node <- nodes) {
        parent(node) = node
        size(node) = 1
    }

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

    def getComponentCount: Int = componentCount

    def getComponentSize(node: String): Int = size(find(node))
}

def findCut(edges: List[(String, String)], r: Random): Group = {
    val allNodes = Set(edges.flatMap { case (u, v) => List(u, v) }.toSeq*)
    val shuffledEdges = r.shuffle(edges)
    val uf = new UnionFind(allNodes)

    var i = 0

    while (uf.getComponentCount > 2 && i < shuffledEdges.length) {
        val (u, v) = shuffledEdges(i)
        uf.union(u, v)
        i += 1
    }

    var cutSize = 0

    for ((u, v) <- edges) {
        if (uf.find(u) != uf.find(v)) {
            cutSize += 1
        }
    }

    if (uf.getComponentCount != 2) return Group(0, 0, 0)
    
    val components = allNodes.groupBy(uf.find).values.map(_.size).toList
    return Group(cutSize, components.head, components(1))
}

def solver(input: List[String]): Int = {
    val edges = parseEdges(input)
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