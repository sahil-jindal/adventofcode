package day21

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Point(y: Int, x: Int)

def parseInput(input: List[String]): Set[Point] = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
        if ch != '#'
    } yield Point(y, x)).toSet
}

// the double % takes care of negative numbers
def mod(n: Int, m: Int) = ((n % m) + m) % m

def getNeighbours(pos: Point) = List(
    pos.copy(x = pos.x - 1),
    pos.copy(x = pos.x + 1),
    pos.copy(y = pos.y - 1),
    pos.copy(y = pos.y + 1)
)

def step(map: Set[Point], positions: Set[Point]): Set[Point] = {
    return positions.flatMap(getNeighbours).filter(posT => {
        val tileCol = mod(posT.x, 131)
        val tileRow = mod(posT.y, 131)
        map.contains(Point(tileRow, tileCol))
    })
}

def getSteps(map: Set[Point]): Iterator[Int] = {
    return Iterator.iterate(Set(Point(65, 65)))(step(map, _)).map(_.size)
}

def evaluatorOne(input: Set[Point]): Int = getSteps(input).drop(64).next()

def evaluatorTwo(input: Set[Point]): Long = {
    // Exploiting some nice properties of the input it reduces to quadratic 
    // interpolation over 3 points: k * 131 + 65 for k = 0, 1, 2
    // I used the Newton method.
    
    val steps = getSteps(input).take(328).toVector

    val (x0, y0) = (65d, steps(65).toDouble)
    val (x1, y1) = (196d, steps(196).toDouble)
    val (x2, y2) = (327d, steps(327).toDouble)

    val y01 = (y1 - y0) / (x1 - x0)
    val y12 = (y2 - y1) / (x2 - x1)
    val y012 = (y12 - y01) / (x2 - x0)

    val n = 26501365

    return Math.round(y0 + y01 * (n - x0) + y012 * (n - x0) * (n - x1))
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day21.txt") match {
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