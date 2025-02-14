package day25

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable

val currentValuePattern = """\s*If the current value is (\d+):""".r
val writePattern = """\s*- Write the value (\d+)\.""".r
val movePattern = """\s*- Move one slot to the (left|right)\.""".r
val continuePattern = """\s*- Continue with state (\w+)\.""".r

def evaluatorOne(input: List[String]): Int = {
    val lines = input.filter(_.trim.nonEmpty).toList
    
    // Parse initial state and steps
    val initialState = lines.head match {
        case s"Begin in state $state." => state
    }
    
    val steps = lines(1) match {
        case s"Perform a diagnostic checksum after $n steps." => n.toInt
    }

    // Parse transitions
    val transitions = mutable.Map.empty[String, Map[Int, (Int, Int, String)]]
    val linesIterator = lines.drop(2).iterator

    while (linesIterator.hasNext) {
        val stateLine = linesIterator.next().trim
        val state = stateLine.split(" ")(2).stripSuffix(":")
        var stateTrans = Map.empty[Int, (Int, Int, String)]

        for (_ <- 0 until 2) {
            val currentValueLine = linesIterator.next().trim
            val currentValue = currentValuePattern.findFirstMatchIn(currentValueLine).get.group(1).toInt
            
            val writeLine = linesIterator.next().trim
            val writeValue = writePattern.findFirstMatchIn(writeLine).get.group(1).toInt
            
            val moveLine = linesIterator.next().trim
        
            val direction = moveLine match {
                case movePattern("left") => -1
                case movePattern("right") => 1
            }

            val continueLine = linesIterator.next().trim
            val nextState = continuePattern.findFirstMatchIn(continueLine).get.group(1)
            
            stateTrans += (currentValue -> (writeValue, direction, nextState))
        }

        transitions(state) = stateTrans
    }

    // Simulation
    
    val tape = mutable.Map.empty[Int, Int]
    var currentPos = 0
    var currentState = initialState

    for (_ <- 0 until steps) {
        val currentValue = tape.getOrElse(currentPos, 0)
        val (writeValue, dir, nextState) = transitions(currentState)(currentValue)

        if (writeValue == 0) {
            tape.remove(currentPos)
        } else {
            tape(currentPos) = writeValue
        }

        currentPos += dir
        currentState = nextState
    }

    return tape.size
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day25.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
        } 
        case Failure(exception) => {
            println(s"File not found: ${exception.getMessage()}")
        }
    }
}