package day09

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Node(value: Int, var left: Node = null, var right: Node = null)

def solver(input: String, mul: Int): Long = {
    val Seq(length, currPoints) = raw"(\d+)".r.findAllIn(input).map(_.toInt).toSeq
    val players = Array.ofDim[Long](length)
    val targetPoints = currPoints * mul

    var current = new Node(value = 0)
    current.left = current
    current.right = current

    val iplayer = Iterator.continually(players.indices).flatten

    for (points <- 1 to targetPoints) {
        if (points % 23 == 0) {
            for (_ <- 0 until 7) do current = current.left

            players(iplayer.next()) += points + current.value

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
    }

    return players.max
}

def evaluatorOne(input: String): Long = solver(input, 1)
def evaluatorTwo(input: String): Long = solver(input, 100)

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