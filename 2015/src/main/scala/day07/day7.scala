package day07

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map => MutableMap}

def parseInput(input: List[String]) = input.map(line => {
    val words = raw"(\w+)".r.findAllIn(line).toList
    words.last -> words.init
}).toMap

def evaluate(wire: String, instructions: Map[String, List[String]], cache: MutableMap[String, Int]): Int = {
    if (cache.contains(wire)) return cache(wire)
    if (wire.forall(_.isDigit)) return wire.toInt

    val value = instructions(wire) match {
        case List(a, "AND", b) => evaluate(a, instructions, cache) & evaluate(b, instructions, cache)
        case List(a, "OR", b) => evaluate(a, instructions, cache) | evaluate(b, instructions, cache)
        case List(a, "LSHIFT", n) => evaluate(a, instructions, cache) << n.toInt
        case List(a, "RSHIFT", n) => evaluate(a, instructions, cache) >> n.toInt
        case List("NOT", a) => ~evaluate(a, instructions, cache) & 0xFFFF
        case s => evaluate(s.mkString, instructions, cache) // Direct assignment
    }

    cache(wire) = value
    value
}

def solver(input: List[String]): (Int, Int) = {
    val originalInstructions = parseInput(input)
    val signalA = evaluate("a", originalInstructions, MutableMap.empty)
    
    val modifiedInstructions = originalInstructions + ("b" -> List(signalA.toString))
    val newSignalA = evaluate("a", modifiedInstructions, MutableMap.empty)
    
    return (signalA, newSignalA)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day07.txt") match {
        case Success(lines) => {
            val (partOne, partTwo) = solver(lines)
            println(s"Part One: $partOne")
            println(s"Part Two: $partTwo")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}