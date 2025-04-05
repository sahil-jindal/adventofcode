package day09

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Set, Queue}

case class Point(x: Int, y: Int)

def parseInput(map: List[String]): Map[Point, Int] = {
    return (for { 
        y <- 0 until map.length; 
        x <- 0 until map(0).length
    } yield Point(x, y) -> map(y)(x).asDigit ).toMap
}

def neighbours(point: Point): Seq[Point] = {
    return Seq(
        point.copy(y = point.y + 1),
        point.copy(y = point.y - 1),
        point.copy(x = point.x + 1),
        point.copy(x = point.x - 1)
    )
}

def getLowPoints(map: Map[Point, Int]): Seq[Point] = map.keys.filter(point =>
    neighbours(point).forall { neighbor => map.getOrElse(neighbor, 9) > map(point) }
).toSeq


def basicInSize(map: Map[Point, Int], point: Point): Int = {
    val filled = Set(point)
    val queue = Queue(point)

    while (queue.nonEmpty) {
        val current = queue.dequeue()
        
        for {
            neighbor <- neighbours(current)
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
        .sorted(Ordering[Int].reverse)
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