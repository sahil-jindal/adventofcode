package day18

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{PriorityQueue, Set}

case class Point(y: Int, x: Int) {
    override def toString(): String = s"$x,$y"
}

def parseInput(input: List[String]) = input.map(line => {
    val Seq(x, y) = raw"(\d+)".r.findAllIn(line).map(_.toInt).toSeq
    Point(y, x)
})

def getNeighbour(pos: Point) = Seq(
    pos.copy(x = pos.x - 1),
    pos.copy(x = pos.x + 1),
    pos.copy(y = pos.y - 1),
    pos.copy(y = pos.y + 1)
)

def distance(blocks: List[Point]): Option[Int] = {
    val size = 70
    val start = Point(0, 0)
    val goal = Point(size, size)

    val blocked = Set((blocks :+ start)*)

    val pq = PriorityQueue((start, 0))(using Ordering.by[(Point, Int), Int](_._2).reverse)

    while (pq.nonEmpty) {
        val (pos, dist) = pq.dequeue()

        if (pos == goal) return Some(dist)

        for (posT <- getNeighbour(pos)) {
            if (!blocked.contains(posT) &&
                0 <= posT.y && posT.y <= size &&
                0 <= posT.x && posT.x <= size
            ) {
                pq.enqueue((posT, dist + 1))
                blocked += posT
            }
        }
    }

    return None
}

def evaluatorOne(blocks: List[Point]): Int = distance(blocks.take(1024)).get

def evaluatorTwo(blocks: List[Point]): String = {
    // find the first block position that will cut off the goal position
    // we can use a binary search for this

    var (lo, hi) = (0, blocks.length)

    while (hi - lo > 1) {
        val m = (lo + hi) / 2

        if (distance(blocks.take(m)).isEmpty) {
            hi = m
        } else {
            lo = m
        }
    }

    return blocks(lo).toString()
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day18.txt") match {
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