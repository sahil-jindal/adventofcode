package day17

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

def solve(input: String): List[Int] = {
    val Seq(xMin, xMax, yMin, yMax) = raw"(-?\d+)".r.findAllIn(input).map(_.toInt).toSeq
    
    // Bounds for the initial horizontal and vertical speeds:
    val vx0Min = 0      // Because vx is non negative
    val vx0Max = xMax   // For bigger values we jump too much to the right in the first step
    val vy0Min = yMin   // For smaller values we jump too deep in the first step
    val vy0Max = -yMin  // when the falling probe reaches y = 0, it's speed is -vy0, which should not be deeper than yMin.

    val res = ListBuffer.empty[Int]

    for (vx0 <- vx0Min to vx0Max; vy0 <- vy0Min to vy0Max) {
        var (x, y, vx, vy) = (0, 0, vx0, vy0)
        var maxY = 0

        breakable {
            while (x <= xMax && y >= yMin) {
                x += vx
                y += vy
                vy -= 1
                vx = math.max(0, vx - 1)
                maxY = math.max(y, maxY)

                if (x >= xMin && x <= xMax && y >= yMin && y <= yMax) {
                    res += maxY
                    break()
                }
            }
        }
    }

    return res.toList
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day17.txt") match {
        case Success(lines) => {
            val result = solve(lines.head)
            println(s"Part One: ${result.max}")
            println(s"Part Two: ${result.size}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}