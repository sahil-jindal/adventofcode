package day24

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

extension (a: Int) {
    def saturatingAdd(b: Int): Int = {
        val sum = a.toLong + b.toLong
        if (sum > Int.MaxValue) return Int.MaxValue
        if (sum < Int.MinValue) return Int.MinValue
        return sum.toInt
    }
}

extension (a: Long) {
    def saturatingMul(b: Long): Long = {
        val r = BigInt(a) * BigInt(b)
        if (r > Long.MaxValue) return Long.MaxValue
        if (r < Long.MinValue) return Long.MinValue
        return r.toLong
    }
}

def parseInput(input: List[String]) = input.map(_.toInt).sorted

def solver(numbers: List[Int], groupLength: Int): Long = {
    val goal = numbers.sum / groupLength

    val minimum = Array.fill(goal + 1)(Int.MaxValue)
    minimum(0) = 0

    val qe = Array.fill(goal + 1)(Long.MaxValue)
    qe(0) = 1

    for (item <- numbers; i <- goal to item by -1) {
        val take = minimum(i - item).saturatingAdd(1)
        val notTake = minimum(i)

        if (take < notTake) {
            // Taking the item result in fewer packages, use the new quantum entanglement 
            // even if it's greater than the existing value.
            minimum(i) = take
            qe(i) = qe(i - item).saturatingMul(item)
        } else if (take == notTake) {
            // Number of packages is the same, so choose the minimum quantum entanglement.
            qe(i) = qe(i).min(qe(i - item).saturatingMul(item))
        }
    }

    return qe(goal)
}

def evaluatorOne(numbers: List[Int]): Long = solver(numbers, 3)
def evaluatorTwo(numbers: List[Int]): Long = solver(numbers, 4)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day24.txt") match {
        case Success(lines) => {
            val numbers = parseInput(lines)
            println(s"Part One: ${evaluatorOne(numbers)}")
            println(s"Part Two: ${evaluatorTwo(numbers)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}