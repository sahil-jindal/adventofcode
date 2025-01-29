package day13

import scala.collection.mutable.{Queue, Set}

val Direction = List((-1, 0), (0, 1), (1, 0), (1, 0))

case class Point(val y: Int, val x: Int)
case class PointCost(val steps: Int, currPoint: Point)

def steps(input: Int): Iterator[(Int, Int, Int)] =
    val q = Queue((0, 1, 1))
    val seen = Set((1, 1))
    val directions = Seq((-1, 0), (1, 0), (0, -1), (0, 1))

    Iterator.continually {
      if q.isEmpty then Iterator.empty else
        val (steps, row, col) = q.dequeue()
        
        val nextMoves = directions.flatMap { case (drow, dcol) =>
            val (newRow, newCol) = (row + drow, col + dcol)
            if newRow >= 0 && newCol >= 0 && !seen.contains((newRow, newCol)) then
                val w = newCol * newCol + 3 * newCol + 2 * newCol * newRow + newRow + newRow * newRow + input
                if w.toBinaryString.count(_ == '1') % 2 == 0 then
                    seen.add((newRow, newCol))
                    Some((steps + 1, newRow, newCol))
                else None
            else None
        }

        q.enqueueAll(nextMoves)
        Iterator.single((steps, row, col))
    
    }.flatten

def partOne(input: String): Int = steps(input.toInt)
    .find { case (_, row, col) => row == 39 && col == 31 }
    .map(_._1)
    .getOrElse(-1)

def partTwo(input: String): Int = steps(input.toInt)
    .takeWhile(_._1 <= 50)
    .size

def hello() = 
    println(s"Part One: ${partOne("1350")}")
    println(s"Part Two: ${partTwo("1350")}")
