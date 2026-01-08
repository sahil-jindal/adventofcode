package day05

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Stack

case class Command(count: Int, from: Int, to: Int)
case class Input(crates: List[List[Char]], commands: List[Command])
case class Move(count: Int, source: Stack[Char], target: Stack[Char])

def parseInput(input: List[String]): Input = {
    val idx = input.indexWhere(_.trim.isEmpty)
    val stackDefs = input.take(idx)
    val moveDefs = input.drop(idx + 1)

    val maxlen = stackDefs.map(_.length).max

    val crates = stackDefs.init.reverse
        .map(_.padTo(maxlen, ' ').grouped(4).map(_(1)).toList)
        .transpose.map(_.takeWhile(_.isLetter))
            
    val commands = moveDefs.map(line => {
        val Seq(count, start, end) = raw"(\d+)".r.findAllIn(line).map(_.toInt).toSeq
        Command(count, start - 1, end - 1)
    })   
    
    return Input(crates, commands)
}

def moveCrates(input: Input, crateMover: Move => Unit): String = {
    val stacks = input.crates.map(it => Stack.from(it.reverse))

    for (Command(count, from, to) <- input.commands) {
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

def evaluatorOne(input: Input): String = moveCrates(input, crateMoverOne)
def evaluatorTwo(input: Input): String = moveCrates(input, crateMoverTwo)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day05.txt") match {
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