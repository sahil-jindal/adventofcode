package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def knotHash(input: Vector[Int], rounds: Int): Vector[Int] = {
    var knot = (0 until 256).toVector
    var (position, skip) = (0, 0)

    for (_ <- 0 until rounds; len <- input) {
        val next = len + skip

        // Reverse the vector from 0 until length
        val (a, b) = knot.splitAt(len)
        knot = a.reverse ++ b

        // Rotating the vector to the left by (next % 256)
        val (c, d) = knot.splitAt(next % 256)
        knot = d ++ c

        position += next
        skip += 1
    }
    
    // Rotating the vector to the right by (position % 256)
    val temp = position % 256
    return knot.takeRight(temp) ++ knot.dropRight(temp)
}

def evaluatorOne(input: String): Int = {
    val chars = input.split(",").map(_.toInt).toVector
    val hash = knotHash(chars, 1)
    return hash(0) * hash(1)
}

def evaluatorTwo(input: String): String = {
    val suffix = Vector(17, 31, 73, 47, 23)
    val chars = input.map(_.toInt).toVector ++ suffix
    val hash = knotHash(chars, 64)
    
    return hash.grouped(16)
        .map(_.reduce(_ ^ _))
        .map(n => f"$n%02x")
        .mkString
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day10.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines.head)}")
            println(s"Part Two: ${evaluatorTwo(lines.head)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}