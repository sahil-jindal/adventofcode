package day11

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Set, Queue}

case class Pos(x: Int, y: Int)

def parseInput(lines: List[String]): Map[Pos, Int] = {
    return (for {
        y <- lines.indices
        x <- lines(y).indices
    } yield Pos(x, y) -> lines(y)(x).asDigit).toMap
}

def neighbours(pos: Pos): Seq[Pos] = {
    return (for {
        dx <- -1 to 1
        dy <- -1 to 1
        if dx != 0 || dy != 0
    } yield Pos(pos.x + dx, pos.y + dy))
}

def step(map: Map[Pos, Int]): (Map[Pos, Int], Int) = {
    var updatedMap = map.view.mapValues(_ + 1).toMap
    val queue = Queue(updatedMap.collect { case (p, e) if e > 9 => p }.toSeq*)
    val flashed = Set.empty[Pos]

    while (queue.nonEmpty) {
        val pos = queue.dequeue()
        if (!flashed.contains(pos)) {
            flashed += pos
            for (n <- neighbours(pos) if updatedMap.contains(n)) {
                updatedMap += n -> (updatedMap(n) + 1)
                if (updatedMap(n) == 10) queue.enqueue(n)
            }
        }
    }

    val resetMap = updatedMap.map {
        case (pos, _) if flashed.contains(pos) => pos -> 0
        case other => other
    }

    return (resetMap, flashed.size)
}

def simulate(current: Map[Pos, Int]): LazyList[Int] = {
    val (next, flashes) = step(current)
    return flashes #:: simulate(next)
}

def evaluatorOne(input: Map[Pos, Int]): Int = simulate(input).take(100).sum
def evaluatorTwo(input: Map[Pos, Int]): Int = simulate(input).indexWhere(_ == 100) + 1

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