package day04

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Direction(dy: Int, dx: Int) {
    def *(num: Int) = Direction(dy * num, dx * num)
    def +(that: Direction) = Direction(dy + that.dy, dx + that.dx)
}

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

type Grid = Map[Point, Char]

val U = Direction(-1, 0)
val D = Direction(1, 0)
val L = Direction(0, -1)
val R = Direction(0, 1)

def parseInput(input: List[String]): Grid = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Point(y, x) -> ch).toMap
}

def matches(map: Grid, pt: Point, dir: Direction, pattern: String): Boolean = {
    val str = pattern.indices.map(i => map.getOrElse(pt + dir * i, '-')).mkString
    return str == pattern || str == pattern.reverse
}

def evaluatorOne(input: Grid): Int = {
    return (for {
        pt <- input.keys.toSeq
        dir <- Seq(R, R + D, D + L, D)
        if matches(input, pt, dir, "XMAS")
    } yield 1).sum
}

def evaluatorTwo(input: Grid): Int = {
    return input.keys.count(pt => {
        val m1 = matches(input, pt + U + L, D + R, "MAS")
        val m2 = matches(input, pt + D + L, U + R, "MAS")
        m1 && m2
    })
}

def readinputFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readinputFromFile("day04.txt") match {
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