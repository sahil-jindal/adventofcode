package day15

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{PriorityQueue, Map => MutableMap}

case class Point(y: Int, x: Int)

def parseInput(input: List[String]): Map[Point, Int] = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Point(y, x) -> ch.asDigit).toMap
}

def getNeighbours(pos: Point) = Seq(
    pos.copy(x = pos.x - 1),
    pos.copy(x = pos.x + 1),
    pos.copy(y = pos.y + 1),
    pos.copy(y = pos.y - 1)
)

def solve(riskMap: Map[Point, Int]): Int = {
    val topLeft = Point(0, 0)
    val maxY = riskMap.keys.map(_.y).max
    val maxX = riskMap.keys.map(_.x).max
    val bottomRight = Point(maxY, maxX)

    val pq = PriorityQueue.empty(using Ordering.by[(Point, Int), Int](_._2).reverse)
    val totalRiskMap = MutableMap.empty[Point, Int]

    totalRiskMap(topLeft) = 0
    pq.enqueue((topLeft, 0))

    while (pq.nonEmpty) {
        val (p, _) = pq.dequeue()

        if (p == bottomRight) {
            return totalRiskMap(p)
        }

        for (n <- getNeighbours(p) if riskMap.contains(n)) {
            val totalRiskThroughP = totalRiskMap(p) + riskMap(n)
            if (totalRiskThroughP < totalRiskMap.getOrElse(n, Int.MaxValue)) {
                totalRiskMap(n) = totalRiskThroughP
                pq.enqueue((n, totalRiskThroughP))
            }
        }
    }

    return totalRiskMap(bottomRight)
}

def scaleUp(map: Map[Point, Int]): Map[Point, Int] = {
    val width = map.keys.map(_.x).max + 1
    val height = map.keys.map(_.y).max + 1

    return (for {
        y <- 0 until 5*height
        x <- 0 until 5*width
        tileY = y % height
        tileX = x % width
        tileRiskLevel = map(Point(tileY, tileX))
        tileDistance = (y / height) + (x / width)
        riskLevel = (tileRiskLevel + tileDistance - 1) % 9 + 1
    } yield Point(y, x) -> riskLevel).toMap
}

def evaluatorOne(map: Map[Point, Int]): Int = solve(map)
def evaluatorTwo(map: Map[Point, Int]): Int = solve(scaleUp(map))

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day15.txt") match {
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