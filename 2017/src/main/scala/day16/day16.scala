package day16

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map, ListBuffer}

sealed trait Move
case class Spin(size: Int) extends Move
case class Exchange(a: Int, b: Int) extends Move
case class Partner(a: Char, b: Char) extends Move

def parseInput(input: String) = input.split(',').map(move => {
    move(0) match {
        case 's' => Spin(move.tail.toInt)
        case 'x' => {
            val parts = move.tail.split('/')
            Exchange(parts(0).toInt, parts(1).toInt)
        }
        case 'p' => {
            val parts = move.tail.split('/')
            Partner(parts(0).head, parts(1).head)
        }
        case _ => throw new IllegalArgumentException(s"Unknown move: $move")
    }
}).toList

def applySpin(s: String, x: Int): String = {
    val splitPos = s.length - x
    return s.substring(splitPos) + s.substring(0, splitPos)
}

def applyExchange(s: String, a: Int, b: Int): String = {
    val arr = s.toCharArray
    val temp = arr(a)
    arr(a) = arr(b)
    arr(b) = temp
    return new String(arr)
}

def applyPartner(s: String, a: Char, b: Char): String = {
    val idxA = s.indexOf(a)
    val idxB = s.indexOf(b)
    return applyExchange(s, idxA, idxB)
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