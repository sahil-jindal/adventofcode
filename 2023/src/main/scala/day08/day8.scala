package day08

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

enum Cmd { case Left, Right }

case class Pair(dirs: IndexedSeq[Cmd], network: Map[String, Map[Cmd, String]])

def parseInput(input: List[String]): Pair = {
    val dirs = input.head.collect {
        case 'L' => Cmd.Left
        case 'R' => Cmd.Right
    }

    val network = input.drop(2).map(line => {
        val Seq(key, left, right) = raw"(\w+)".r.findAllIn(line).toSeq
        key -> Map(Cmd.Left -> left, Cmd.Right -> right)
    }).toMap

    return Pair(dirs, network)
}

def stepsToZ(input: Pair, currentInit: String, zMarker: String): Long = {
    val Pair(dirsInit, network) = input
    val dirs = Iterator.continually(dirsInit).flatten
    var current = currentInit
    var i = 0L

    while (!current.endsWith(zMarker)) {
        current = network(current)(dirs.next())
        i += 1
    }

    return i
}

def gcd(a: Long, b: Long): Long = if b == 0 then a else gcd(b, a % b)
def lcm(a: Long, b: Long): Long = a * b / gcd(a, b)

def solve(input: Pair, aMarker: String, zMarker: String): Long = {
    return input.network.keys.filter(_.endsWith(aMarker)).map(stepsToZ(input, _, zMarker)).reduce(lcm)
}

def evaluatorOne(input: Pair): Long = solve(input, "AAA", "ZZZ")
def evaluatorTwo(input: Pair): Long = solve(input, "A", "Z")

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