package day12

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, ListBuffer, Set => MutableSet}

case class Direction(dy: Int, dx: Int)

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

type Region = Set[Point]

val U = Direction(-1, 0)
val L = Direction(0, -1)
val D = Direction(1, 0)
val R = Direction(0, 1)

def getRegions(input: List[String]): List[Region] = {
    val gardern = (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Point(y, x) -> ch).toMap

    val res = ListBuffer.empty[Region]
    val positions = MutableSet(gardern.keySet.toSeq*)

    while (positions.nonEmpty) {
        val pivot = positions.head
        val region = MutableSet(pivot)

        val q = Queue(pivot)
        val plant = gardern(pivot)

        while (q.nonEmpty) {
            val point = q.dequeue()
            positions.remove(point)

            for (dir <- Seq(U, R, D, L)) {
                val neighbor = point + dir
                if (!region.contains(neighbor) && gardern.getOrElse(neighbor, ' ') == plant) {
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
    return Seq(U, R, D, L).count(du => !region.contains(pt + du))
}

def findCorners(region: Region, pt: Point): Int = {
    var res = 0

    for ((du, dv) <- Seq((U, R), (R, D), (D, L), (L, U))) {
        if (!region.contains(pt + du) &&
            !region.contains(pt + dv)
        ) {
            res += 1
        }

        if (region.contains(pt + du) &&
            region.contains(pt + dv) &&
            !region.contains(pt + du + dv)
        ) {
            res += 1
        }
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