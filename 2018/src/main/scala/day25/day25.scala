package day25

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Vec4D(x: Int, y: Int, z: Int, w: Int) {
    def manhattanDistance(other: Vec4D): Int = {
        (x - other.x).abs + (y - other.y).abs + (z - other.z).abs + (w - other.w).abs
    }
}

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
    val Array(x, y, z, w) = line.split(",").map(_.toInt)
    Vec4D(x, y, z, w)
})

def formConstellations(points: List[Vec4D]): Int = {
    val uf = DisjointUnionSets(points.length)
    
    for {
        i <- points.indices
        j <- i + 1 until points.length
        if points(i).manhattanDistance(points(j)) <= 3
    } uf.union(i, j)

    return points.indices.map(uf.find).toSet.size
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day25.txt") match {
        case Success(lines) => println(s"Part One: ${formConstellations(parseInput(lines))}")
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}