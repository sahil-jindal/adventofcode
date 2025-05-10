package day17

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{ListBuffer, PriorityQueue, Set}

case class Complex(real: Int, imag: Int) {
    def unary_- = Complex(-real, -imag)
    def +(that: Complex) = Complex(real + that.real, imag + that.imag)
    def -(that: Complex) = Complex(real - that.real, imag - that.imag)
}

case class Crucible(pos: Complex, dir: Complex, straight: Int)

type Grid = Map[Complex, Int]

def parseInput(input: List[String]): Grid = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Complex(x, y) -> ch.asDigit).toMap
}

def moves(c: Crucible, minStraight: Int, maxStraight: Int): Seq[Crucible] = {
    val result = ListBuffer.empty[Crucible]
    
    // Continue straight if we haven't exceeded maxStraight
    if (c.straight < maxStraight) {
        result += Crucible(c.pos + c.dir, c.dir, c.straight + 1)
    }
    
    // Turn left or right if we've gone at least minStraight
    if (c.straight >= minStraight) {
        // Turn left (90 degrees counterclockwise)
        val leftDir = Complex(-c.dir.imag, c.dir.real)  // Multiply by i
        result += Crucible(c.pos + leftDir, leftDir, 1)
      
        // Turn right (90 degrees clockwise)
        val rightDir = Complex(c.dir.imag, -c.dir.real)  // Multiply by -i
        result += Crucible(c.pos + rightDir, rightDir, 1)
    }
    
    result.toSeq
}

def heatloss(map: Grid, minStraight: Int, maxStraight: Int): Int = {
    val goal = map.keys.maxBy(pos => pos.imag + pos.real)
    
    val pq = PriorityQueue.empty(using Ordering.by[(Crucible, Int), Int](_._2).reverse)
    val seen = Set.empty[Crucible]

    pq.enqueue((Crucible(pos = Complex(0, 0), dir = Complex(1, 0), straight = 0), 0))
    pq.enqueue((Crucible(pos = Complex(0, 0), dir = Complex(0, 1), straight = 0), 0))

    while (pq.nonEmpty) {
        val (crucible, heatloss) = pq.dequeue()

        if (crucible.pos == goal && crucible.straight >= minStraight) return heatloss

        for (next <- moves(crucible, minStraight, maxStraight)) {
            if (map.contains(next.pos) && !seen.contains(next)) {
                seen.add(next)
                pq.enqueue((next, heatloss + map(next.pos)))
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