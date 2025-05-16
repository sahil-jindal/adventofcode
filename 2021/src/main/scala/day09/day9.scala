package day09

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Set, Queue}

case class Point(y: Int, x: Int)

def parseInput(map: List[String]): Map[Point, Int] = {
    return (for { 
        (line, y) <- map.zipWithIndex 
        (ch, x) <- line.zipWithIndex 
    } yield Point(y, x) -> ch.asDigit).toMap
}

def getNeighbours(pos: Point) = Seq(
    pos.copy(x = pos.x - 1),
    pos.copy(x = pos.x + 1),
    pos.copy(y = pos.y - 1),
    pos.copy(y = pos.y + 1)
)

def getLowPoints(map: Map[Point, Int]): Seq[Point] = map.keys.filter(point =>
    getNeighbours(point).forall { neighbor => map.getOrElse(neighbor, 9) > map(point) }
).toSeq


def basicInSize(map: Map[Point, Int], point: Point): Int = {
    val filled = Set(point)
    val queue = Queue(point)

    while (queue.nonEmpty) {
        val current = queue.dequeue()
        
        for {
            neighbor <- getNeighbours(current)
            if !filled.contains(neighbor)
            if map.getOrElse(neighbor, 9) != 9
        } {
            filled += neighbor
            queue.enqueue(neighbor)
        }
    }

    return filled.size
}

def evaluatorOne(map: Map[Point, Int]): Int = {
    return getLowPoints(map).map(point => 1 + map(point)).sum
}

def evaluatorTwo(map: Map[Point, Int]): Int = {
    return getLowPoints(map)
        .map(p => basicInSize(map, p))
        .sorted(using Ordering.Int.reverse)
        .take(3)
        .product
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day09.txt") match {
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