package day24

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{PriorityQueue, Set}

case class Pos(time: Int, y: Int, x: Int)

case class Maps(private val map: List[String]) {    
    val (height, width) = (map.size, map(0).size)

    def get(pos: Pos): Char = {
        if (pos.y == 0 && pos.x == 1) return '.'
        if (pos.y == height - 1 && pos.x == width - 2) return '.'

        if (pos.y <= 0 || pos.y >= height - 1 || 
            pos.x <= 0 || pos.x >= width - 1) return '#'

        // blizzards have a horizontal and a vertical loop
        // it's easy to check the original postions with going back in time
        // using modular arithmetic
        
        val hmod = width - 2
        val vmod = height - 2

        val xW = (pos.x - 1 + hmod - (pos.time % hmod)) % hmod + 1
        val xE = (pos.x - 1 + hmod + (pos.time % hmod)) % hmod + 1
        val yN = (pos.y - 1 + vmod - (pos.time % vmod)) % vmod + 1
        val yS = (pos.y - 1 + vmod + (pos.time % vmod)) % vmod + 1

        if (map(pos.y)(xW) == '>') return '>'
        if (map(pos.y)(xE) == '<') return '<'
        if (map(yN)(pos.x) == 'v') return 'v'
        if (map(yS)(pos.x) == '^') return '^'
        
        return '.'
    }
}

case class Triplet(entry: Pos, exit: Pos, maps: Maps)

def parseInput(input: List[String]): Triplet = {
    val maps = Maps(input)
    val entry = Pos(0, 0, 1)
    val exit = Pos(Int.MaxValue, maps.height - 1, maps.width - 2)
    return Triplet(entry, exit, maps)
}

def nextPositions(posInit: Pos, maps: Maps): List[Pos] = {
    val pos = posInit.copy(time = posInit.time + 1)

    val neighbours = List(
        pos,
        pos.copy(y = pos.y - 1),
        pos.copy(y = pos.y + 1),
        pos.copy(x = pos.x - 1),
        pos.copy(x = pos.x + 1)
    )
    
    return neighbours.filter(it => maps.get(it) == '.')
}

// Standard A* algorithm
def walkTo(start: Pos, goal: Pos, maps: Maps): Pos = {

    def evaluation(pos: Pos): Int = pos.time + (goal.y - pos.y).abs + (goal.x - pos.x).abs

    val pq = PriorityQueue(start)(using Ordering.by(evaluation).reverse)
    val seen = Set.empty[Pos]

    while (pq.nonEmpty) {
        var pos = pq.dequeue()

        if (pos.y == goal.y && pos.x == goal.x) return pos

        val positions = nextPositions(pos, maps).filterNot(seen.contains)
        seen.addAll(positions)
        pq.addAll(positions)
    }

    throw Exception("No solution found")
}

def solver(input: Triplet): (Int, Int) = {
    val Triplet(entry, exit, maps) = input
    var pos = walkTo(entry, exit, maps)
    val partOne = pos.time

    pos = walkTo(pos, entry, maps)
    pos = walkTo(pos, exit, maps)
    val partTwo = pos.time

    return (partOne, partTwo)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day24.txt") match {
        case Success(lines) => {
            val (partOne, partTwo) = solver(parseInput(lines))
            println(s"Part One: ${partOne}")
            println(s"Part Two: ${partTwo}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}