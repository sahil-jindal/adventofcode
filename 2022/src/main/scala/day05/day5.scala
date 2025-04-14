package day05

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Stack

case class Move(count: Int, source: Stack[Char], target: Stack[Char])

val moveRegex = raw"move (\d+) from (\d+) to (\d+)".r

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def moveCrates(input: List[String], crateMover: Move => Unit): String = {
    val List(stackDefs, moveDefs) = groupLines(input)

    val numStacks = stackDefs.last.split("\\s+").filter(_.nonEmpty).length
    val stacks = List.fill(numStacks)(Stack.empty[Char])

    for {
        line <- stackDefs.init.reverse
        (stack, item) <- stacks.zip(line.grouped(4))
        if item(1) != ' '
    } stack.push(item(1))
            
    for (line <- moveDefs) {
        val nums = moveRegex.findFirstMatchIn(line).get.subgroups.map(_.toInt)
        val count = nums(0)
        val from = nums(1) - 1
        val to = nums(2) - 1
        crateMover(Move(count, stacks(from), stacks(to)))
    }   
    
    return stacks.map(_.pop()).mkString
}

def crateMoverOne(move: Move): Unit = {
    for (_ <- 0 until move.count) {
        move.target.push(move.source.pop())
    }
}

def crateMoverTwo(move: Move): Unit = {
    // Two stacks makes a Queue
    var helper = Stack.empty[Char]

    for (_ <- 0 until move.count) {
        helper.push(move.source.pop())
    }

    for (_ <- 0 until move.count) {
        move.target.push(helper.pop())
    }
}

def evaluatorOne(input: List[String]): String = moveCrates(input, crateMoverOne)
def evaluatorTwo(input: List[String]): String = moveCrates(input, crateMoverTwo)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day05.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}