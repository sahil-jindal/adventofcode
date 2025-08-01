package day12

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, ListBuffer, Set => MutableSet}

case class Direction(dy: Int, dx: Int) {
    def +(that: Direction) = Direction(dy + that.dy, dx + that.dx)
}

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

type Region = Set[Point]

val N = Direction(-1, 0)
val W = Direction(0, -1)
val S = Direction(1, 0)
val E = Direction(0, 1)
val NE = N + E
val NW = N + W
val SE = S + E
val SW = S + W

def getRegions(input: List[String]): List[Region] = {
    val garden = (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Point(y, x) -> ch).toMap

    val res = ListBuffer.empty[Region]
    val positions = MutableSet.from(garden.keySet)

    while (positions.nonEmpty) {
        val pivot = positions.head
        val region = MutableSet(pivot)

        val q = Queue(pivot)
        val plant = garden(pivot)

        while (q.nonEmpty) {
            val point = q.dequeue()
            positions.remove(point)

            for (neighbor <- List(N, E, S, W).map(point + _).filter(garden.contains)) {
                if (!region.contains(neighbor) && garden(neighbor) == plant) {
                    region.add(neighbor)
                    q.enqueue(neighbor)
                }
            }
        }

        res += region.toSet
    }

    return res.toList
}

def calculateFencePrice(regions: List[Region], measure: (Region, Point) => Int): Int = {
    return regions.map(it => it.iterator.map(pt => measure(it, pt)).sum * it.size).sum
}

def findEdges(region: Region, pt: Point): Int = {    
    return List(N, E, S, W).count(du => !region.contains(pt + du))
}

def findCorners(region: Region, pt: Point): Int = {
    var res = 0

    for (corner <- List(List(S, SE, E), List(W, SW, S), List(N, NW, W), List(E, NE, N))) {
        val List(a, b, c) = corner.map(pt + _)
        if (!region.contains(a) && !region.contains(c)) res += 1
        if (region.contains(a) && region.contains(c) && !region.contains(b)) res += 1
    }

    return res
}

def evaluatorOne(regions: List[Region]): Int = calculateFencePrice(regions, findEdges)
def evaluatorTwo(regions: List[Region]): Int = calculateFencePrice(regions, findCorners)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day12.txt") match {
        case Success(lines) => {
            val regions = getRegions(lines)
            println(s"Part One: ${evaluatorOne(regions)}")
            println(s"Part Two: ${evaluatorTwo(regions)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}