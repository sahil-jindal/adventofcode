package day11

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, Set, Map => MutableMap}

case class Point(y: Int, x: Int)

type Grid = Map[Point, Int]

def parseInput(input: List[String]): Grid = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Point(y, x) -> ch.asDigit).toMap
}

def neighbours(pos: Point): Seq[Point] = {
    return (for {
        dy <- -1 to 1
        dx <- -1 to 1
        if dy != 0 || dx != 0
    } yield Point(pos.y + dy, pos.x + dx))
}

def step(grid: Grid): (Grid, Int) = {
    val updatedMap = MutableMap(grid.view.mapValues(_ + 1).toSeq*)
    val queue = Queue(updatedMap.collect { case (p, e) if e > 9 => p }.toSeq*)
    val flashed = Set.empty[Point]

    while (queue.nonEmpty) {
        val pos = queue.dequeue()
        if (!flashed.contains(pos)) {
            flashed += pos
            for (n <- neighbours(pos) if updatedMap.contains(n)) {
                updatedMap(n) += 1
                if (updatedMap(n) == 10) queue.enqueue(n)
            }
        }
    }

    val resetMap = updatedMap.map {
        case (pos, _) if flashed.contains(pos) => pos -> 0
        case other => other
    }

    return (resetMap.toMap, flashed.size)
}

def simulate(current: Grid): LazyList[Int] = {
    val (next, flashes) = step(current)
    return flashes #:: simulate(next)
}

def evaluatorOne(input: Grid): Int = simulate(input).take(100).sum
def evaluatorTwo(input: Grid): Int = simulate(input).indexWhere(_ == 100) + 1

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day11.txt") match {
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