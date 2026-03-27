package day12

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Direction(dy: Int, dx: Int) {
    def unary_- = Direction(-dy, -dx)
    def rotateLeft = Direction(-dx, dy)
    def rotateRight = Direction(dx, -dy)
    def *(num: Int) = Direction(dy * num, dx * num)
    def +(dir: Direction) = Direction(dy + dir.dy, dx + dir.dx)
}

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

type Pair = (action: Char, arg: Int)

def parseInput(input: List[String]) = input.map(line => (line.head, line.tail.toInt))

def rotate(dir: Direction, amount: Int): Direction = {
    return math.floorMod(amount, 360) match {
        case 90 => dir.rotateRight
        case 180 => -dir
        case 270 => dir.rotateLeft
        case _: Int => throw Exception()
    }
}

def evaluatorOne(input: List[Pair]): Int = {
    var (pos, dir) = (Point(0, 0), Direction(0, 1))

    for ((command, amount) <- input) {
        command match {
            case 'N' => pos += Direction(-amount, 0)
            case 'S' => pos += Direction(amount, 0)
            case 'E' => pos += Direction(0, amount)
            case 'W' => pos += Direction(0, -amount)
            case 'L' => dir = rotate(dir, -amount)
            case 'R' => dir = rotate(dir, amount)
            case 'F' => pos += dir * amount
        }
    }

    return pos.x.abs + pos.y.abs
}

def evaluatorTwo(input: List[Pair]): Int = {
    var (pos, way) = (Point(0, 0), Direction(-1, 10))

    for ((command, amount) <- input) {
        command match {
            case 'N' => way += Direction(-amount, 0)
            case 'S' => way += Direction(amount, 0)
            case 'E' => way += Direction(0, amount)
            case 'W' => way += Direction(0, -amount)
            case 'L' => way = rotate(way, -amount)
            case 'R' => way = rotate(way, amount)
            case 'F' => pos += way * amount
        }
    }

    return pos.x.abs + pos.y.abs
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day12.txt") match {
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