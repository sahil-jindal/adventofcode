package day13

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Vec2D(x: Long, y: Long) {
    def *(num: Long) = Vec2D(x * num, y * num)
    def +(num: Long) = Vec2D(x + num, y + num)
    def +(that: Vec2D) = Vec2D(x + that.x, y + that.y)
}

case class Machine(a: Vec2D, b: Vec2D, p: Vec2D)

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def parseInput(input: List[String]): List[Machine] = {
    return groupLines(input).map(group => {
        val List(a, b, p) = group.map(line => {
            val Seq(x, y) = raw"(\d+)".r.findAllIn(line).map(_.toLong).toSeq
            Vec2D(x, y)
        })

        Machine(a, b, p)
    })
}

def det(a: Vec2D, b: Vec2D): Long = a.x * b.y - a.y * b.x

def getPrize(m: Machine): Long = {
    val Machine(a, b, p) = m

    val D = det(a, b)

    if (D == 0) return 0L

    // solve a * i + b * j = p for i and j using Cramer's rule
    val i = det(p, b) / D
    val j = det(a, p) / D

     // return the prize when a non negative _integer_ solution is found
    if (i >= 0 && j >= 0 && a * i + b * j == p) return 3 * i + j
    
    return 0L
}

def solver(machines: List[Machine], shift: Long): Long = {
    return machines.map(m => m.copy(p = m.p + shift)).map(getPrize).sum
}

def evaluatorOne(machines: List[Machine]): Long = solver(machines, 0L)
def evaluatorTwo(machines: List[Machine]): Long = solver(machines, 10000000000000L)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day13.txt") match {
        case Success(lines) => {
            val regions = parseInput(lines)
            println(s"Part One: ${evaluatorOne(regions)}")
            println(s"Part Two: ${evaluatorTwo(regions)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}