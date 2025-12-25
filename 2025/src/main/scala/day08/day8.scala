package day08

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.control.Breaks._

case class Edge(a: Int, b: Int)
case class PairOne(dist: Long, edge: Edge)

case class Vec3D(x: Long, y: Long, z: Long) {
    def distance(other: Vec3D): Long = {
        val dx = x - other.x
        val dy = y - other.y
        val dz = z - other.z
        return dx*dx + dy*dy + dz*dz
    }
}

type PairTwo = (first: List[Vec3D], second: IndexedSeq[Edge])

class DisjointUnionSets(size: Int) {
    private val parent = (0 until size).toArray
    private val rank = Array.fill(size)(1)

    def find(p: Int): Int = {
        if (parent(p) != p) {
            parent(p) = find(parent(p)) // Path compression
        }

        return parent(p)
    }

    def union(p: Int, q: Int): Unit = {
        val rootP = find(p)
        val rootQ = find(q)
        
        if (rootP != rootQ) {
            if (rank(rootP) > rank(rootQ)) {
                parent(rootQ) = rootP
            } else if (rank(rootP) < rank(rootQ)) {
                parent(rootP) = rootQ
            } else {
                parent(rootQ) = rootP
                rank(rootP) += 1
            }
        }
    }
}

def parseInput(input: List[String]) = input.map(line => {
    val Array(x, y, z) = line.split(",").map(_.toLong)
    Vec3D(x, y, z)
})

def preComputation(points: List[Vec3D]): PairTwo = {
    val pairs = for {
        i <- 0 to points.length - 2
        j <- i + 1 to points.length - 1
    } yield PairOne(points(i).distance(points(j)), Edge(i, j))

    return (points, pairs.sortBy(_.dist).map(_.edge))
}

def evaluatorOne(pair: PairTwo): Int = {
    val (points, sortedEdges) = pair
    
    val uf = DisjointUnionSets(points.length)

    sortedEdges.take(1000).foreach { case Edge(a, b) => uf.union(a, b) }

    return points.indices.groupMapReduce(uf.find)(_ => 1)(_ + _)
        .values.toSeq.sorted(using Ordering.Int.reverse).take(3).product
}

def evaluatorTwo(pair: PairTwo): Long = {
    val (points, sortedEdges) = pair
    
    val uf = DisjointUnionSets(points.length)

    val maybeEdge = sortedEdges.find(it => {
        uf.union(it.a, it.b)
        points.indices.distinctBy(uf.find).size == 1
    })

    val Edge(a, b) = maybeEdge.get
    return points(a).x * points(b).x
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day08.txt") match {
        case Success(lines) => {
            val input = preComputation(parseInput(lines))
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}