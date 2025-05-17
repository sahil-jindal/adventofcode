package day08

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Pair(left: String, right: String)

type Edges = Map[String, Pair]

def parseMap(input: List[String]): Edges = {
    return input.map(line => {
        val parts = raw"(\w+)".r.findAllIn(line).toList
        parts(0) -> Pair(parts(1), parts(2))
    }).toMap
}

def stepsToZ(currentInit: String, zMarker: String, dirsInit: String, map: Edges): Long = {
    val dirs = Iterator.continually(dirsInit).flatten
    var current = currentInit
    var i = 0L

    while (!current.endsWith(zMarker)) {
        val dir = dirs.next()
        current = if dir == 'L' then map(current).left else map(current).right
        i += 1
    }

    return i
}

def gcd(a: Long, b: Long): Long = if b == 0 then a else gcd(b, a % b)
def lcm(a: Long, b: Long): Long = a * b / gcd(a, b)

def solve(input: List[String], aMarker: String, zMarker: String): Long = {
    val dirs = input.head
    val map = parseMap(input.drop(2))

    return map.keys.collect { case w if w.endsWith(aMarker) => stepsToZ(w, zMarker, dirs, map) }.reduce(lcm)
}

def evaluatorOne(input: List[String]): Long = solve(input, "AAA", "ZZZ")
def evaluatorTwo(input: List[String]): Long = solve(input, "A", "Z")

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day08.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}