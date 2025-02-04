package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.annotation.tailrec

def knotHash(input: List[Int], rounds: Int): Array[Int] = {
    val output = (0 until 256).toArray
    
    @tailrec
    def loop(current: Int, skip: Int, remainingRounds: Int): Unit = {
        if remainingRounds <= 0 then return
      
        var newCurrent = current
        var newSkip = skip
        
        input.foreach { len =>
            for i <- 0 until (len / 2) do {
                val from = (newCurrent + i) % output.length
                val to = (newCurrent + len - 1 - i) % output.length
                val temp = output(from)
                output(from) = output(to)
                output(to) = temp
            }
        
            newCurrent = (newCurrent + len + newSkip) % output.length
            newSkip += 1
        }
      
        loop(newCurrent, newSkip, remainingRounds - 1)
    }
    
    loop(0, 0, rounds)
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

@main
def hello(): Unit =
    readLinesFromFile("day10.txt") match
        case Success(lines) => {
            println(s"Part One: ${partOne(lines(0))}")
            println(s"Part Two: ${partTwo(lines(0))}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }