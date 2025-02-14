package day03

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Set

case class Rectangle(val claimId: Int, val startX: Int, val startY: Int, val w: Int, val h: Int)

val rx = raw"""(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r

def parseInput(lines: List[String]): List[Rectangle] = lines.map(line => {
    val Vector(a, b, c, d, e) = rx.findFirstMatchIn(line).get.subgroups.map(_.toInt).toVector
    Rectangle(a, b, c, d, e)
})

def decorate(input: List[Rectangle]): Unit = {
    val mtx = Array.fill(1000, 1000)(0)
    var overlapArea = 0
    val ids = Set[Int]()

    input.foreach { rec =>
        ids.add(rec.claimId)

        for(
            i <- rec.startX until (rec.startX + rec.w);
            j <- rec.startY until (rec.startY + rec.h)
        ) {
            mtx(i)(j) match {
                case 0 => mtx(i)(j) = rec.claimId
                case -1 => ids.remove(rec.claimId)
                case otherId => {
                    ids.remove(otherId)
                    ids.remove(rec.claimId)
                    overlapArea += 1
                    mtx(i)(j) = -1
                }
            }
        }
    }
    
    println(s"Part One: $overlapArea")
    println(s"Part Two: ${ids.head}")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day03.txt") match {
        case Success(lines) => {
            decorate(parseInput(lines))
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}