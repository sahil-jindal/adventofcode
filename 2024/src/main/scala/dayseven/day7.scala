package dayseven

import scala.util.{Try, Success, Failure}
import scala.io.Source

def parseLine(line: String): (Long, List[Long]) = {
    val parts = line.split(": ")
    val target = parts(0).toLong
    val numbers = parts(1).split(" ").map(_.toLong).toList
    (target, numbers)
}

def canMatchTarget(target: Long, numbers: List[Long], current: Long = 0): Boolean = {
    if (numbers.isEmpty) return current == target

    val next = numbers.head
    val rest = numbers.tail

    canMatchTarget(target, rest, current + next) ||
    canMatchTarget(target, rest, current * next)
}

def canMatchTargetSecond(target: Long, numbers: List[Long], current: Long = 0): Boolean = {
    if (numbers.isEmpty) return current == target

    val next = numbers.head
    val rest = numbers.tail

    val a = canMatchTargetSecond(target, rest, current + next)
    val b = canMatchTargetSecond(target, rest, current * next)
    
    val temp = s"${current.toString()}${next.toString()}".toLong
    val c = canMatchTargetSecond(target, rest, temp)

    a || b || c
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Try {
        val source = Source.fromFile(filePath)
        
        try { 
            source.getLines().toList
        } finally { 
            source.close()
        }
    }

@main
def hello(): Unit =
    readLinesFromFile("src/main/scala/dayseven/file.txt") match
        case Success(lines) => {        
            val equations = lines.map(parseLine)
            val calibrationResult = equations.collect {
                case (target, numbers) if canMatchTargetSecond(target, numbers.tail, numbers.head) => target
            }.sum
            println(s"The total calibration result is $calibrationResult")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }