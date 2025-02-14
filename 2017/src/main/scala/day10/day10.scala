package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def knotHash(input: List[Int], rounds: Int): Array[Int] = {
    val output = (0 until 256).toArray
    var (current, skip) = (0, 0)

    for (_ <- 0 until rounds) do {
        for (len <- input) do {
            for (i <- 0 until len / 2) do {
                val from = (current + i) % output.length
                val to = (current + len - 1 - i) % output.length
                val temp = output(from)
                output(from) = output(to)
                output(to) = temp
            }
            
            current += len + skip
            skip += 1
        }
    }

    return output
}

def partOne(input: String): Int = {
    val chars = input.split(",").map(_.toInt).toList
    val hash = knotHash(chars, 1)
    return hash(0) * hash(1)
}

def partTwo(input: String): String = {
    val suffix = List(17, 31, 73, 47, 23)
    val chars = input.map(_.toInt).toList ++ suffix
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
            println(s"Part One: ${partOne(lines.head)}")
            println(s"Part Two: ${partTwo(lines.head)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}