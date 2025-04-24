package day09

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Node(val value: Int, var left: Node = null, var right: Node = null)

val matchRegex = "(\\d+) players; last marble is worth (\\d+) points".r

def solver(line: String, mul: Int): Long = {
    val m = matchRegex.findFirstMatchIn(line).get
    var players = Array.ofDim[Long](m.group(1).toInt)
    var targetPoints = m.group(2).toInt * mul

    var current = new Node(value = 0)
    current.left = current
    current.right = current

    var points = 1
    var iplayer = 1

    while (points <= targetPoints) {
        if (points % 23 == 0) {
            for (_ <- 0 until 7) do current = current.left

            players(iplayer) += points + current.value

            val left = current.left
            val right = current.right
            right.left = left
            left.right = right
            current = right
        } else {
            val left = current.right
            val right = current.right.right
            current = Node(value = points, left = left, right = right)
            left.right = current
            right.left = current
        }

        points += 1
        iplayer = (iplayer + 1) % players.length
    }

    return players.max
}

def evaluatorOne(line: String): Long = solver(line, 1)
def evaluatorTwo(line: String): Long = solver(line, 100)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day09.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines.head)}")
            println(s"Part Two: ${evaluatorTwo(lines.head)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}