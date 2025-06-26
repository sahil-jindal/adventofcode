package day20

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.CollectionConverters._

case class Point(y: Int, x: Int) {
    def manhattan(that: Point) = (x - that.x).abs + (y - that.y).abs
}

def getNeighbours(pos: Point) = Seq(
    pos.copy(x = pos.x - 1),
    pos.copy(x = pos.x + 1),
    pos.copy(y = pos.y - 1),
    pos.copy(y = pos.y + 1),
)

def getPath(input: List[String]): List[Point] = {
    val grid = (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
        if ch != '#'
    } yield Point(y, x) -> ch).toMap

    val start = grid.collectFirst { case (pos, ch) if ch == 'S' => pos }.get
    val goal = grid.collectFirst { case (pos, ch) if ch == 'E' => pos }.get

    var prev: Point = null
    var cur = goal

    val res = ListBuffer(cur)

    while (cur != start) {
        val newPos = getNeighbours(cur).find(p => grid.contains(p) && p != prev).get
        prev = cur
        cur = newPos
        res += cur
    }

    return res.toList
}

// Depending upon the hardware, this would atmost take 45 mins to 1hr
def solve(path: List[Point], cheat: Int): Int = {
    def cheatsFromI(i: Int): Int = {
        return (0 to i).count(j => {
            val dist = path(i).manhattan(path(j))
            val saving = i - j - dist
            dist <= cheat && saving >= 100
        })
    }

    return path.indices.par.map(cheatsFromI).sum
}

def evaluatorOne(path: List[Point]): Int = solve(path, 2)
def evaluatorTwo(path: List[Point]): Int = solve(path, 20)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day20.txt") match {
        case Success(lines) => {
            val input = getPath(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}