package day24

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Set, PriorityQueue}

case class Pos(time: Int, y: Int, x: Int)

case class Maps(private val map: List[String]) {    
    val height = map.size
    val width = map(0).size

    def get(pos: Pos): Char = {
        if (pos.y == 0 && pos.x == 1) then return '.'
        if (pos.y == height - 1 && pos.x == width - 2) then return '.'

        if (pos.y <= 0 || pos.y >= height - 1 || 
            pos.x <= 0 || pos.x >= width - 1
        ) {
            return '#'
        }

        // blizzards have a horizontal and a vertical loop
        // it's easy to check the original postions with going back in time
        // using modular arithmetic
        val hmod = width - 2
        val vmod = height - 2

        val xW = (pos.x - 1 + hmod - (pos.time % hmod)) % hmod + 1
        val xE = (pos.x - 1 + hmod + (pos.time % hmod)) % hmod + 1
        val yN = (pos.y - 1 + vmod - (pos.time % vmod)) % vmod + 1
        val yS = (pos.y - 1 + vmod + (pos.time % vmod)) % vmod + 1

        if map(pos.y)(xW) == '>' then return '>'
        if map(pos.y)(xE) == '<' then return '<'
        if map(yN)(pos.x) == 'v' then return 'v'
        if map(yS)(pos.x) == '^' then return '^'
        
        return '.';
    }
}

def parseInput(input: List[String]): (Pos, Pos, Maps) = {
    val maps = Maps(input)
    val entry = Pos(0, 0, 1)
    val exit = Pos(Int.MaxValue, maps.height - 1, maps.width - 2)
    return (entry, exit, maps)
}

def nextPositions(posInit: Pos, maps: Maps): Seq[Pos] = {
    val pos = posInit.copy(time = posInit.time + 1)

    val neighbours = Seq(
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

    val pq = PriorityQueue.empty(Ordering.by(evaluation).reverse)
    val seen = Set.empty[Pos]

    pq.enqueue(start)

    while (pq.nonEmpty) {
        var pos = pq.dequeue()

        if (pos.y == goal.y && pos.x == goal.x) return pos

        for (nextPos <- nextPositions(pos, maps)) {
            if (!seen.contains(nextPos)) {
                seen.add(nextPos)
                pq.enqueue(nextPos)
            }
        }
    }

    throw Exception("No solution found")
}

def solver(input: List[String]): Unit = {
    val (entry, exit, maps) = parseInput(input)
    var pos = walkTo(entry, exit, maps)
    println(s"Part One: ${pos.time}")

    pos = walkTo(pos, entry, maps)
    pos = walkTo(pos, exit, maps)
    println(s"Part Two: ${pos.time}")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day24.txt") match {
        case Success(lines) => solver(lines)
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}