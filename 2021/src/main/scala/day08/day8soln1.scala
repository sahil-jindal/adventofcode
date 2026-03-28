package day08

import scala.util.{Success, Failure}

def decodeOutput(line: String): List[Int] = {
    val Array(first, second) = line.split(" \\| ").map(_.split(" ").toList)

    val patterns = first.map(_.toSet)

    val digits = Array.ofDim[Set[Char]](10)

    digits(1) = patterns.find(_.size == 2).get
    digits(4) = patterns.find(_.size == 4).get

    def lookup(segmentCount: Int, commonWithOne: Int, commonWithFour: Int) = {
        patterns.find(pattern => 
            pattern.size == segmentCount && 
            (pattern & digits(1)).size == commonWithOne && 
            (pattern & digits(4)).size == commonWithFour
        ).get
    }

    digits(0) = lookup(6, 2, 3)  
    digits(2) = lookup(5, 1, 2)
    digits(3) = lookup(5, 2, 3)
    digits(5) = lookup(5, 1, 3)
    digits(6) = lookup(6, 1, 3)
    digits(7) = lookup(3, 2, 2)
    digits(8) = lookup(7, 2, 4)
    digits(9) = lookup(6, 2, 4)

    def decode(v: String) = digits.indexOf(v.toSet)

    return second.map(decode)
}

def parseInputOne(input: List[String]) = input.map(decodeOutput)

def evaluatorOneSoln1(input: List[List[Int]]): Int = input.flatten.count(Set(2, 3, 4, 7).contains)
def evaluatorTwoSoln1(input: List[List[Int]]): Int = input.map(_.mkString.toInt).sum

def solutionOne(): Unit = {
    readLinesFromFile("day08.txt") match {
        case Success(lines) => {
            val input = parseInputOne(lines)
            println(s"Part One: ${evaluatorOneSoln1(input)}")
            println(s"Part Two: ${evaluatorTwoSoln1(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}