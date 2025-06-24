package day18

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Group(dir: Char, amount: Int, color: String)

case class Direction(dy: Long, dx: Long) {
    def *(num: Int) = Direction(dy * num, dx * num)
    def magnitude = Math.sqrt(dy.toDouble * dy.toDouble + dx.toDouble * dx.toDouble)
}

case class Point(y: Long, x: Long) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

def parseInput(input: List[String]) = input.map(line => {
    val Array(a, b, c) = line.split(" ")
    Group(a.head, b.toInt, c.substring(2, 8))
})

def stepsOne(input: List[Group]) = input.map(group => {
    val dir = group.dir match {
        case 'R' => Direction(0, 1)
        case 'U' => Direction(-1, 0)
        case 'L' => Direction(0, -1)
        case 'D' => Direction(1, 0)
    }

    dir * group.amount
})

def stepsTwo(input: List[Group]) = input.map(group => {
    val dir = group.color.last match {
        case '0' => Direction(0, 1)
        case '1' => Direction(1, 0)
        case '2' => Direction(0, -1)
        case '3' => Direction(-1, 0)
    }

    dir * Integer.parseInt(group.color.init, 16) 
})

// We are using a combination of the shoelace formula with Pick's theorem
def area(steps: List[Direction]): Long = {
    val vertices = steps.scanLeft(Point(0, 0))(_ + _).tail

    // Shoelace formula https://en.wikipedia.org/wiki/Shoelace_formula
    val shiftedVertices = vertices.tail :+ vertices.head

    val shoelaces = (vertices zip shiftedVertices).map { 
        case (p1, p2) => p1.x * p2.y - p1.y * p2.x
    }

    val area = shoelaces.sum.abs / 2

    // Pick's theorem  https://en.wikipedia.org/wiki/Pick%27s_theorem
    val boundary = steps.map(_.magnitude).sum.toLong
    val interior = area - (boundary / 2) + 1

    // integer area
    return boundary + interior
}

def evaluatorOne(input: List[Group]): Long = area(stepsOne(input))
def evaluatorTwo(input: List[Group]): Long = area(stepsTwo(input))

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day18.txt") match {
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