package day05

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Stack

type Command = (count: Int, from: Int, to: Int)
type Input = (crates: Vector[List[Char]], commands: List[Command])
type Move = (count: Int, source: Stack[Char], target: Stack[Char])

def parseInput(input: List[String]): Input = {
    val idx = input.indexWhere(_.trim.isEmpty)
    val stackDefs = input.take(idx)
    val moveDefs = input.drop(idx + 1)

    val maxlen = stackDefs.map(_.length).max

    val crates = stackDefs.init.reverse
        .map(_.padTo(maxlen, ' ').grouped(4).map(_(1)).toList)
        .transpose.map(_.takeWhile(_.isLetter)).toVector
            
    val commands = moveDefs.collect {
        case s"move $a from $b to $c" => (a.toInt, b.toInt - 1, c.toInt - 1)
    }   
    
    return (crates, commands)
}

def moveCrates(input: Input, crateMover: Move => Unit): String = {
    val (crates, commands) = input
    val stacks = crates.map(it => Stack.from(it.reverse))

    for ((count, from, to) <- commands) {
        crateMover((count, stacks(from), stacks(to)))
    }

    return stacks.map(_.pop()).mkString
}

def crateMoverOne(move: Move): Unit = {
    val (count, source, target) = move

    for (_ <- 0 until count) {
        target.push(source.pop())
    }
}

def crateMoverTwo(move: Move): Unit = {
    val (count, source, target) = move

    // Two stacks makes a Queue
    val helper = Stack.empty[Char]

    for (_ <- 0 until count) {
        helper.push(source.pop())
    }

    for (_ <- 0 until count) {
        target.push(helper.pop())
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