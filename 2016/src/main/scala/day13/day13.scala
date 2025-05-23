package day13

import scala.collection.mutable.{Queue, Set}

val Direction = List((-1, 0), (0, 1), (1, 0), (1, 0))

case class Point(y: Int, x: Int)
case class Group(steps: Int, y: Int, x: Int)

def steps(input: Int): Iterator[Group] = {
    val q = Queue(Group(0, 1, 1))
    val seen = Set((1, 1))
    val directions = Seq((-1, 0), (1, 0), (0, -1), (0, 1))

    Iterator.continually {
        if q.isEmpty then Iterator.empty else {
            val Group(steps, y, x) = q.dequeue()
            
            val nextMoves = directions.flatMap { case (dy, dx) =>
                val (newY, newX) = (y + dy, x + dx)
                if newY < 0 || newX < 0 || seen.contains((newY, newX)) then None else {
                    val w = newX * newX + 3 * newX + 2 * newX * newY + newY + newY * newY + input
                    if w.toBinaryString.count(_ == '1') % 2 != 0 then None else {
                        seen.add((newY, newX))
                        Some(Group(steps + 1, newY, newX))
                    }
                }
            }

            q.enqueueAll(nextMoves)
            Iterator.single(Group(steps, y, x))
        }
    }.flatten
}

def evaluatorOne(input: Int): Int = steps(input).collectFirst { case Group(steps, y, x) if y == 39 && x == 31 => steps }.get
def evaluatorTwo(input: Int): Int = steps(input).takeWhile(_.steps <= 50).size

def hello(): Unit = {
    val inputLine = 1350
    println(s"Part One: ${evaluatorOne(inputLine)}")
    println(s"Part Two: ${evaluatorTwo(inputLine)}")
}