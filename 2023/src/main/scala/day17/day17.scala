package day17

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{ListBuffer, PriorityQueue, Set}

case class Direction(dy: Int, dx: Int) {
    def unary_- = Direction(-dy, -dx)
    def rotateLeft = Direction(dx, -dy)
    def rotateRight = Direction(-dx, dy)
}

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

case class Crucible(pos: Point, dir: Direction, straight: Int)

type Grid = Map[Point, Int]

def parseInput(input: List[String]): Grid = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Point(y, x) -> ch.asDigit).toMap
}

def moves(c: Crucible, minStraight: Int, maxStraight: Int): List[Crucible] = {
    val result = ListBuffer.empty[Crucible]
    
    // Continue straight if we haven't exceeded maxStraight
    if (c.straight < maxStraight) {
        result += Crucible(c.pos + c.dir, c.dir, c.straight + 1)
    }
    
    // Turn left or right if we've gone at least minStraight
    if (c.straight >= minStraight) {
        // Turn left (90 degrees counterclockwise)
        val leftDir = c.dir.rotateLeft  // Multiply by i
        result += Crucible(c.pos + leftDir, leftDir, 1)
      
        // Turn right (90 degrees clockwise)
        val rightDir = c.dir.rotateRight  // Multiply by -i
        result += Crucible(c.pos + rightDir, rightDir, 1)
    }
    
    return result.toList
}

def heatloss(grid: Grid, minStraight: Int, maxStraight: Int): Int = {
    val goal = grid.keys.maxBy(pos => pos.y + pos.x)
    
    val pq = PriorityQueue.empty(using Ordering.by[(Crucible, Int), Int](_._2).reverse)
    val seen = Set.empty[Crucible]

    pq.enqueue((Crucible(pos = Point(0, 0), dir = Direction(1, 0), straight = 0), 0))
    pq.enqueue((Crucible(pos = Point(0, 0), dir = Direction(0, 1), straight = 0), 0))

    while (pq.nonEmpty) {
        val (crucible, heatloss) = pq.dequeue()

        if (crucible.pos == goal && crucible.straight >= minStraight) return heatloss

        for (next <- moves(crucible, minStraight, maxStraight)) {
            if (grid.contains(next.pos) && seen.add(next)) {
                pq.enqueue((next, heatloss + grid(next.pos)))
            }
        }
    }

    throw Exception("No solution found")
}

def evaluatorOne(input: Grid): Int = heatloss(input, 0, 3)
def evaluatorTwo(input: Grid): Int = heatloss(input, 4, 10)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day17.txt") match {
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