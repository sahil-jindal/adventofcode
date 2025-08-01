package day15

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Pos(x: Int, y: Int) {
    def manhattan(other: Pos): Int = (x - other.x).abs + (y - other.y).abs
}

case class Rect(x: Int, y: Int, width: Int, height: Int) {
    val left = x
    val top = y
    val right = x + width - 1
    val bottom = y + height - 1

    def corners = List(
        Pos(left, top),
        Pos(right, top),
        Pos(left, bottom),
        Pos(right, bottom)
    )

    def split(): List[Rect] = {
        val w0 = width / 2
        val w1 = width - w0
        val h0 = height / 2
        val h1 = height - h0

        return List(
            Rect(left, top, w0, h0),
            Rect(left + w0, top, w1, h0),
            Rect(left, top + h0, w0, h1),
            Rect(left + w0, top + h0, w1, h1)    
        )
    }
}

case class Pair(sensor: Pos, beacon: Pos) {
    val radius = sensor.manhattan(beacon)
    def inRange(pos: Pos): Boolean = pos.manhattan(sensor) <= radius
    def toRect = Rect(sensor.x - radius, sensor.y - radius, 2 * radius + 1, 2 * radius + 1)
}

def parseInput(input: List[String]) = input.map(line => {
    val Seq(sx, sy, bx, by) = raw"(-?\d+)".r.findAllIn(line).map(_.toInt).toSeq
    Pair(Pos(sx, sy), Pos(bx, by))
})

def getUncoveredAreas(pairing: List[Pair], rect: Rect): List[Rect] = {
    if (rect.width <= 0 || rect.height <= 0) return List.empty[Rect]

    val coveredByCorners = pairing.exists(pair => rect.corners.forall(pair.inRange))
    if (coveredByCorners) return List.empty[Rect]

    if (rect.width == 1 && rect.height == 1) return List(rect)

    return rect.split().flatMap(getUncoveredAreas(pairing, _))
}

def evaluatorOne(pairing: List[Pair]): Int = {
    val rects = pairing.map(_.toRect)

    val left = rects.map(_.left).min
    val right = rects.map(_.right).max
    val y = 2000000

    return (left to right).map(x => Pos(x, y)).count(pos => {
        pairing.exists(pair => pair.beacon != pos && pair.inRange(pos))
    })
}

def evaluatorTwo(pairing: List[Pair]): Long = {
    val area = getUncoveredAreas(pairing, Rect(0, 0, 4000001, 4000001)).head
    return area.x * 4000000L + area.y
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day15.txt") match {
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