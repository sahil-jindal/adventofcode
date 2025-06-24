package day08

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class PairOne(left: String, right: String)
case class PairTwo(dirs: String, map: Edges)

type Edges = Map[String, PairOne]

def parseInput(input: List[String]): PairTwo = {
    val dirs = input.head

    val map = input.drop(2).map(line => {
        val Seq(key, left, right) = raw"(\w+)".r.findAllIn(line).toSeq
        key -> PairOne(left, right)
    }).toMap

    return PairTwo(dirs, map)
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

def solve(input: PairTwo, aMarker: String, zMarker: String): Long = {
    val PairTwo(dirs, map) = input
    return map.keys.collect { case w if w.endsWith(aMarker) => stepsToZ(w, zMarker, dirs, map) }.reduce(lcm)
}

def evaluatorOne(input: PairTwo): Long = solve(input, "AAA", "ZZZ")
def evaluatorTwo(input: PairTwo): Long = solve(input, "A", "Z")

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day08.txt") match {
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