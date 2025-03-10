package day03

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Set

val rx = raw"""(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r

case class Rectangle(val claimId: Int, val startX: Int, val startY: Int, val w: Int, val h: Int)

def parseInput(lines: List[String]): List[Rectangle] = lines.map(line => {
    val List(a, b, c, d, e) = rx.findFirstMatchIn(line).get.subgroups.map(_.toInt)
    Rectangle(a, b, c, d, e)
})

def decorate(input: List[Rectangle]): (Int, Int) = {
    val mtx = Array.fill(1000, 1000)(0)
    val ids = Set.empty[Int]
    var overlapArea = 0

    input.foreach { case Rectangle(claimId, startX, startY, w, h) =>
        ids.add(claimId)

        for(i <- startX until (startX + w); j <- startY until (startY + h)) {
            mtx(i)(j) match {
                case 0 => mtx(i)(j) = claimId
                case -1 => ids.remove(claimId)
                case otherId => {
                    ids.remove(otherId)
                    ids.remove(claimId)
                    overlapArea += 1
                    mtx(i)(j) = -1
                }
            }
        }
    }
    
    return (overlapArea, ids.head)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day03.txt") match {
        case Success(lines) => {
            val (overlapArea, id) = decorate(parseInput(lines))
            println(s"Part One: $overlapArea")
            println(s"Part Two: $id")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}