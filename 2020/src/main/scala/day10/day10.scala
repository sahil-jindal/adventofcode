package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: List[String]): List[Int] = {
    val nums = input.map(_.toInt).toList.sorted
    return 0 +: nums :+ (nums.last + 3)
}

def evaluatorOne(jolts: List[Int]): Long = {
    val differences = (jolts.init zip jolts.tail).map { case (a, b) => b - a }
    return differences.count(_ == 1) * differences.count(_ == 3)
}

// Doing reverse dynamic programming here
def evaluatorTwo(jolts: List[Int]): Long = {
    var (a, b, c) = (1L, 0L, 0L)
    
    for (i <- jolts.length - 2 to 0 by -1) {
        val s =
            (if (i + 1 < jolts.length && jolts(i + 1) - jolts(i) <= 3) a else 0) +
            (if (i + 2 < jolts.length && jolts(i + 2) - jolts(i) <= 3) b else 0) +
            (if (i + 3 < jolts.length && jolts(i + 3) - jolts(i) <= 3) c else 0)
      
        c = b
        b = a
        a = s
    }
    
    return a
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day10.txt") match {
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