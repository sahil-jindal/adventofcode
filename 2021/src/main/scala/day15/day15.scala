package day15

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable

case class Point(x: Int, y: Int)

def parseInput(input: List[String]): Map[Point, Int] = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Point(x, y) -> ch.asDigit).toMap
}

def neighbours(point: Point): Seq[Point] = Seq(
    point.copy(y = point.y + 1),
    point.copy(y = point.y - 1),
    point.copy(x = point.x + 1),
    point.copy(x = point.x - 1)
)

def solve(riskMap: Map[Point, Int]): Int = {
    val topLeft = Point(0, 0)
    val bottomRight = Point(riskMap.keys.map(_.x).max, riskMap.keys.map(_.y).max)

    val pq = mutable.PriorityQueue.empty(Ordering.by[(Point, Int), Int](_._2).reverse)
    val totalRiskMap = mutable.Map.empty[Point, Int]

    totalRiskMap(topLeft) = 0
    pq.enqueue((topLeft, 0))

    while (pq.nonEmpty) {
        val (p, _) = pq.dequeue()

        if (p == bottomRight) {
            return totalRiskMap(p)
        }

        for (n <- neighbours(p) if riskMap.contains(n)) {
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
        tileRiskLevel = map(Point(tileX, tileY))
        tileDistance = (y / height) + (x / width)
        riskLevel = (tileRiskLevel + tileDistance - 1) % 9 + 1
    } yield Point(x, y) -> riskLevel).toMap
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