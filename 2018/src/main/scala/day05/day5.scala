package day05

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Stack

def react(polymer: String): String = {
    val stack = Stack.empty[Char]

    for (unit <- polymer) {
        if (stack.nonEmpty && math.abs(stack.top - unit) == 32) then stack.pop()
        else stack.push(unit)
    }
    
    return stack.mkString
}

def evaluatorOne(polymer: String): Int = react(polymer).length

def evaluatorTwo(polymer: String): Int = ('a' to 'z').map { ch =>
    val filteredPolymer = polymer.filterNot(_.toLower == ch)
    react(filteredPolymer).length
}.min

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day05.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines.head)}")
            println(s"Part Two: ${evaluatorTwo(lines.head)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}