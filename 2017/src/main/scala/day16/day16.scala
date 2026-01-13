package day16

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map, ListBuffer}

sealed trait Move { def dance(str: String): String }

case class Spin(size: Int) extends Move {
    override def dance(str: String): String = {
        return str.takeRight(size) + str.dropRight(size)
    }
}

case class Exchange(a: Int, b: Int) extends Move {
    override def dance(str: String): String = {
        val arr = str.toCharArray
        val temp = arr(a)
        arr(a) = arr(b)
        arr(b) = temp
        return arr.mkString
    }
}

case class Partner(a: Char, b: Char) extends Move {
    override def dance(str: String): String = {
        return Exchange(str.indexOf(a), str.indexOf(b)).dance(str)
    }
}

def parseInput(input: String) = input.split(',').collect {
    case s"s$num" => Spin(num.toInt)
    case s"x$a/$b" => Exchange(a.toInt, b.toInt)
    case s"p$a/$b" => Partner(a.head, b.head)
}.toList

def applyMoves(initial: String, moves: List[Move]): String = {
    return moves.foldLeft(initial) { (current, move) => move.dance(current) }
}

def findCycle(initial: String, moves: List[Move]): (List[String], Int, Int) = {   
    val seen = Map.empty[String, Int]
    val states = ListBuffer.empty[String]
    var current = initial
    var step = 0
    
    while (!seen.contains(current)) {
        seen(current) = step
        states += current
        current = applyMoves(current, moves)
        step += 1
    }

    val cycleStart = seen(current)
    val cycleLength = step - cycleStart
    
    return (states.toList, cycleStart, cycleLength)
}

def evaluatorOne(moves: List[Move]): String = applyMoves("abcdefghijklmnop", moves)

def evaluatorTwo(moves: List[Move]): String = {
    val (states, cycleStart, cycleLength) = findCycle("abcdefghijklmnop", moves)
    val totalSteps = 1000000000
    
    if (totalSteps < cycleStart) return states(totalSteps)
    
    val offset = (totalSteps - cycleStart) % cycleLength
    return states(cycleStart + offset)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day16.txt") match {
        case Success(lines) => {
            val instructions = parseInput(lines.head)
            println(s"Part One: ${evaluatorOne(instructions)}")
            println(s"Part Two: ${evaluatorTwo(instructions)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}