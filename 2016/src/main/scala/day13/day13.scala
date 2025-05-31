package day13

import scala.collection.mutable.{Queue, Set}

case class Point(y: Int, x: Int)
case class Pair(steps: Int, pos: Point)

def getNeighbours(pos: Point) = Seq(
    pos.copy(x = pos.x - 1),
    pos.copy(x = pos.x + 1),
    pos.copy(y = pos.y - 1),
    pos.copy(y = pos.y + 1)
)

def steps(input: Int): Iterator[Pair] = {
    val start = Point(1, 1)
    val q = Queue(Pair(0, start))
    val seen = Set(start)

    Iterator.continually {
        if q.isEmpty then Iterator.empty else {
            val Pair(steps, pos) = q.dequeue()
            
            for (newPos <- getNeighbours(pos)) {
                if (newPos.y >= 0 && newPos.x >= 0 && !seen.contains(newPos)) {
                    val Point(y, x) = newPos
                    val w = x*x + 3*x + 2*x*y + y + y*y + input
                    if (w.toBinaryString.count(_ == '1') % 2 == 0) {
                        seen.add(newPos)
                        q.enqueue(Pair(steps + 1, newPos))
                    }
                }
            }

            Iterator.single(Pair(steps, pos))
        }
    }.flatten
}

def evaluatorOne(input: Int): Int = steps(input).collectFirst { case Pair(steps, pos) if pos == Point(39, 31) => steps }.get
def evaluatorTwo(input: Int): Int = steps(input).takeWhile(_.steps <= 50).size

@main
def hello(): Unit = {
    val inputLine = 1350
    println(s"Part One: ${evaluatorOne(inputLine)}")
    println(s"Part Two: ${evaluatorTwo(inputLine)}")
}