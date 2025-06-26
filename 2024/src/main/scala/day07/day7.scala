package day07

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Pair(target: Long, nums: List[Long])

def parseInput(input: List[String]) = input.map(line => {
    val parts = raw"(\d+)".r.findAllIn(line).map(_.toLong).toList
    Pair(parts.head, parts.tail)
})

def filterNums(input: List[Pair], matchFn: (Long, Long, List[Long]) => Boolean): Long = {
    return input.collect { case Pair(target, nums) if matchFn(target, nums.head, nums.tail) => target }.sum
}

def canMatchOne(target: Long, acc: Long, nums: List[Long]): Boolean = {
    if (nums.isEmpty) return acc == target

    val (next, rest) = (nums.head, nums.tail)

    canMatchOne(target, acc + next, rest) ||
    canMatchOne(target, acc * next, rest)
}

def canMatchTwo(target: Long, acc: Long, nums: List[Long]): Boolean = {
    if (acc > target) return false
    if (nums.isEmpty) return acc == target

    val (next, rest) = (nums.head, nums.tail)
    
    val temp = s"${acc}${next}".toLong
    
    canMatchTwo(target, temp, rest) ||
    canMatchTwo(target, acc + next, rest) ||
    canMatchTwo(target, acc * next, rest)
}

def evaluatorOne(input: List[Pair]): Long = filterNums(input, canMatchOne)
def evaluatorTwo(input: List[Pair]): Long = filterNums(input, canMatchTwo)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day07.txt") match {
        case Success(lines) => {
            val input = parseInput(lines)        
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part One: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}