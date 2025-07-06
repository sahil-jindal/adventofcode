package day16

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map, ListBuffer}

sealed trait Move
case class Spin(size: Int) extends Move
case class Exchange(a: Int, b: Int) extends Move
case class Partner(a: Char, b: Char) extends Move

def parseInput(input: String) = input.split(',').map(dance => {
    val (moves, rest) = (dance.head, dance.tail)

    moves match {
        case 's' => Spin(rest.toInt)
        case 'x' => {
            val Array(a, b) = rest.split('/').map(_.toInt)
            Exchange(a, b)
        }
        case 'p' => {
            val Array(a, b) = rest.split('/').map(_.head)
            Partner(a, b)
        }
        case _ => throw new Exception(s"Unknown move: $moves")
    }
}).toList

def applySpin(str: String, num: Int): String = {
    return str.takeRight(num) + str.dropRight(num)
}

def applyExchange(str: String, a: Int, b: Int): String = {
    val arr = str.toCharArray
    val temp = arr(a)
    arr(a) = arr(b)
    arr(b) = temp
    return new String(arr)
}

def applyPartner(str: String, a: Char, b: Char): String = {
    return applyExchange(str, str.indexOf(a), str.indexOf(b))
}

def applyMoves(initial: String, moves: List[Move]): String = {
    return moves.foldLeft(initial) { (current, move) =>
        move match {
            case Spin(x) => applySpin(current, x)
            case Exchange(a, b) => applyExchange(current, a, b)
            case Partner(a, b) => applyPartner(current, a, b)
        }
    }
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