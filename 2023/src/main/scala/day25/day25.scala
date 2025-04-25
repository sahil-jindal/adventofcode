package day25

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.Random
import scala.collection.mutable.{Map, Set}

case class Group(size: Int, c1: Int, c2: Int)

def parse(input: List[String]): Map[String, Set[String]] = {
    val graph = Map.empty[String, Set[String]]

    for (line <- input) {
        val parts = line.split(": ")
        val u = parts(0)
        val nodes = parts(1).split(" ")
        
        for (v <- nodes) {
            graph.getOrElseUpdate(u, Set.empty) += v
            graph.getOrElseUpdate(v, Set.empty) += u
        }
    }

    return graph
}

// https://en.wikipedia.org/wiki/Karger%27s_algorithm
// Karger's algorithm finds a cut of a graph and returns its size. 
// It's not necessarily the minimal cut, because it's a randomized algorithm 
// but it's 'likely' to find the minimal cut in reasonable time. 
// The algorithm is extended to return the sizes of the two components 
// separated by the cut as well.
def findCut(input: List[String], r: Random): Group = {
    val graph = parse(input)
    val componentSize = Map(graph.keys.map(_ -> 1).toSeq*)

    // updates backreferences of oldNode to point to newNode
    def rebind(oldNode: String, newNode: String): Unit = {
        for (n <- graph(oldNode)) {
            while (graph(n).remove(oldNode)) {
                graph(n).add(newNode)
            }
        }
    }

    var id = 0
    
    while (graph.size > 2) {
        // decrease the the number of nodes by one. First select two nodes u 
        // and v connected with an edge. Introduce a new node that inherits 
        // every edge going out of these (excluding the edges between them). 
        // Set the new nodes' component size to the sum of the component 
        // sizes of u and v. Remove u and v from the graph.
        val u = graph.keys.toVector(r.nextInt(graph.size))
        val v = graph(u).toVector(r.nextInt(graph(u).size))
        
        val merged = s"merge-$id"
        id += 1

        val mergedEdges = Set((graph(u).filter(_ != v) ++ graph(v).filter(_ != u)).toSeq*)

        graph(merged) = mergedEdges
        
        rebind(u, merged)
        rebind(v, merged)
        
        componentSize(merged) = componentSize(u) + componentSize(v)

        graph.remove(u)
        graph.remove(v)
    }

    val nodeA = graph.keys.head
    val nodeB = graph.keys.last

    return Group(graph(nodeA).size, componentSize(nodeA), componentSize(nodeB))
}

def solver(input: List[String]): Int = {
    val r = Random(25)

    // run Karger's algorithm until it finds a cut with 3 edges
    var g = findCut(input, r)

    while (g.size != 3) {
        g = findCut(input, r)
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