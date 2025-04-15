package day09

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer

case class Motion(dir: Char, dist: Int)
case class Knot(y: Int, x: Int)

def parseInput(input: List[String]) = input.map(line => {
    val m = raw"(\w) (\d+)".r.findFirstMatchIn(line).get
    Motion(m.group(1).head, m.group(2).toInt) 
})

def moveHead(rope: Array[Knot], dir: Char): Unit = {
    rope(0) = dir match {
        case 'U' => rope(0).copy(y = rope(0).y - 1)
        case 'D' => rope(0).copy(y = rope(0).y + 1)
        case 'L' => rope(0).copy(x = rope(0).x - 1)
        case 'R' => rope(0).copy(x = rope(0).x + 1)
        case _ => throw Exception()
    }

    for(i <- 1 until rope.length) {
        val dy = rope(i - 1).y - rope(i).y
        val dx = rope(i - 1).x - rope(i).x

        if (dy.abs > 1 || dx.abs > 1) {
            rope(i) = Knot(
                rope(i).y + dy.sign,
                rope(i).x + dx.sign
            )
        }
    }
}

def tails(motions: List[Motion], ropelength: Int): List[Knot] = {
    val rope = Array.fill(ropelength)(Knot(0, 0))

    val res = ListBuffer(rope.last)

    motions.foreach { case Motion(dir, dist) => 
        for (i <- 0 until dist) {
            moveHead(rope, dir)
            res += rope.last
        }
    }

    res.toList
}

def evaluatorOne(motions: List[Motion]) = tails(motions, 2).toSet.size
def evaluatorTwo(motions: List[Motion]) = tails(motions, 10).toSet.size

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day09.txt") match {
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