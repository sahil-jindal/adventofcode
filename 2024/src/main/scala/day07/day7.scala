package day07

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def filterNums(input: List[String], matchFn: (Long, Long, List[Long]) => Boolean): Long = {
    return (for {
        line <- input
        parts = line.split(": ")
        target = parts(0).toLong
        nums = parts(1).split(" ").map(_.toLong).toList
        if matchFn(target, nums.head, nums.tail)
    } yield target).sum
}

def canMatchOne(target: Long, acc: Long, nums: List[Long]): Boolean = {
    if (nums.isEmpty) return acc == target

    val next = nums.head
    val rest = nums.tail

    canMatchOne(target, acc + next, rest) ||
    canMatchOne(target, acc * next, rest)
}

def canMatchTwo(target: Long, acc: Long, nums: List[Long]): Boolean = {
    if (acc > target) return false
    if (nums.isEmpty) return acc == target

    val next = nums.head
    val rest = nums.tail

    val temp = s"${acc.toString()}${next.toString()}".toLong
    
    canMatchTwo(target, temp, rest) ||
    canMatchTwo(target, acc + next, rest) ||
    canMatchTwo(target, acc * next, rest)
}

def evaluatorOne(input: List[String]): Long = filterNums(input, canMatchOne)
def evaluatorTwo(input: List[String]): Long = filterNums(input, canMatchTwo)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day07.txt") match {
        case Success(lines) => {        
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part One: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}