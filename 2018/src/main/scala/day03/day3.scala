package day03

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Set

case class Rectangle(claimId: Int, startX: Int, startY: Int, w: Int, h: Int)

def parseInput(input: List[String]) = input.map(line => {
    val Seq(a, b, c, d, e) = raw"(\d+)".r.findAllIn(line).map(_.toInt).toSeq
    Rectangle(a, b, c, d, e)
})

def solver(input: List[Rectangle]): (Int, Int) = {
    val mtx = Array.fill(1000, 1000)(0)
    val ids = Set.empty[Int]
    var overlapArea = 0

    for (Rectangle(claimId, startX, startY, w, h) <- input) {
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
            val (partOne, partTwo) = solver(parseInput(lines))
            println(s"Part One: $partOne")
            println(s"Part Two: $partTwo")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}