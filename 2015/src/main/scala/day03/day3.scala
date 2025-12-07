package day03

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Set

case class Direction(dy: Int, dx: Int)

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

def parseInput(line: String) = line.collect {
    case 'v' => Direction(1, 0)
    case '>' => Direction(0, 1)
    case '<' => Direction(0, -1)
    case '^' => Direction(-1, 0)
}

def run(input: IndexedSeq[Direction], actors: Int): Int = {
    val start = Point(0, 0)
    val seen = Set(start)
    val pos = Array.fill(actors)(start)

    for ((dir, i) <- input.zipWithIndex) {
        val idx = i % actors
        pos(idx) += dir
        seen += pos(idx)
    }
    
    return seen.size
}

def evaluatorOne(input: IndexedSeq[Direction]): Int = run(input, 1)
def evaluatorTwo(input: IndexedSeq[Direction]): Int = run(input, 2)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day03.txt") match {
        case Success(lines) => {
            val input = parseInput(lines.head)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}