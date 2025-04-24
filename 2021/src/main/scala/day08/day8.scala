package day08

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Pair(first: List[String], second: List[String])

def parseInput(input: List[String]) = input.map(line => {
    val Array(first, second) = line.split(" \\| ").map(_.split(" ").toList)
    Pair(first, second)
})

def evaluatorOne(input: List[Pair]): Int = {
    // we can identify digits 1, 7, 4 and 8 by their active segments count:
    val segmentCounts = Seq("cf", "acf", "bcdf", "abcdefg").map(_.length).toSet
    return input.flatMap(_.second).count(it => segmentCounts.contains(it.length))
}

def evaluatorTwo(input: List[Pair]): Int = {
    var res = 0

    for (pair <- input) {
        val patterns = pair.first.map(_.toSet)

        val digits = Array.ofDim[Set[Char]](10)

        digits(1) = patterns.find(_.size == "cf".length).get
        digits(4) = patterns.find(_.size == "bcdf".length).get

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

        def decode(v: String) = (0 until 10).find(i => digits(i) == v.toSet).get

        res += pair.second.foldLeft(0) { case (n, digit) => n * 10 + decode(digit) }
    }

    return res
}

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