package day25

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map => MutableMap}

enum State { case ZERO, ONE }

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def parseBluePrint(group: List[String]) = {
    val (first, rest) = (group.head, group.tail)
    val state = first.trim().stripPrefix("In state ").stripSuffix(":").head

    val stateTrans = MutableMap.empty[State, (State, Int, Char)]

    for (lines <- rest.grouped(rest.size / 2)) {
        val temp1 = lines(0).trim().stripPrefix("If the current value is ").stripSuffix(":").head
        val currentValue = if temp1 == '0' then State.ZERO else State.ONE

        val temp2 = lines(1).trim().stripPrefix("- Write the value ").stripSuffix(".").head
        val writeValue = if temp2 == '0' then State.ZERO else State.ONE
    
        val moveValue = lines(2).trim().stripPrefix("- Move one slot to the ").stripSuffix(".")
        val direction = if moveValue == "left" then -1 else 1

        val nextState = lines(3).trim().stripPrefix("- Continue with state ").stripSuffix(".").head

        stateTrans(currentValue) = (writeValue, direction, nextState)
    }

    state -> stateTrans.toMap
}

def solver(input: List[String]): Int = {
    // Parse initial state and steps
    val initialState = input(0).stripPrefix("Begin in state ").stripSuffix(".").head
    val steps = input(1).stripPrefix("Perform a diagnostic checksum after ").stripSuffix(" steps.").toInt
    
    // Parse transitions
    val transitions = groupLines(input.drop(3)).map(parseBluePrint).toMap

    // Simulation
    val tape = MutableMap.empty[Int, State]
    var currentPos = 0
    var currentState = initialState

    for (_ <- 0 until steps) {
        val currentValue = tape.getOrElse(currentPos, State.ZERO)
        val (writeValue, dir, nextState) = transitions(currentState)(currentValue)

        if (writeValue == State.ZERO) {
            tape -= currentPos
        } else {
            tape += currentPos -> writeValue
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
        case Success(lines) => println(s"Answer: ${solver(lines)}")
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage()}")
    }
}