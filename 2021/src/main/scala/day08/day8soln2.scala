package day08

import scala.util.{Success, Failure}

val fingerPrints = List(42, 17, 34, 39, 30, 37, 41, 25, 49, 45)

def descramble(line: String): List[Int] = {
    val Array(first, second) = line.split(" \\| ").map(_.split(" ").toList)
    val freq = first.flatten.groupMapReduce(identity)(_ => 1)(_ + _)
    return second.map(it => fingerPrints.indexOf(it.map(freq).sum))
}

def parseInputTwo(input: List[String]) = input.map(descramble)

def evaluatorOneSoln2(input: List[List[Int]]): Int = input.flatten.count(Set(2, 3, 4, 7).contains)
def evaluatorTwoSoln2(input: List[List[Int]]): Int = input.map(_.mkString.toInt).sum

def solutionTwo(): Unit = {
    readLinesFromFile("day08.txt") match {
        case Success(lines) => {
            val input = parseInputTwo(lines)
            println(s"Part One: ${evaluatorOneSoln2(input)}")
            println(s"Part Two: ${evaluatorTwoSoln2(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}