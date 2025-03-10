package day25

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Point(x: Int, y: Int, z: Int, w: Int) {
    def manhattanDistance(other: Point): Int = {
        Math.abs(x - other.x) + Math.abs(y - other.y) + Math.abs(z - other.z) + Math.abs(w - other.w)
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

def parseInput(lines: List[String]): List[Point] = lines.map(line => {
    val Array(x, y, z, w) = line.split(",").map(_.toInt)
    Point(x, y, z, w)
})

def formConstellations(points: List[Point]): Int = {
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