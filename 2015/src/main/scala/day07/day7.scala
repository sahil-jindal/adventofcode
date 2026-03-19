package day07

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map => MutableMap}

sealed trait Gate
case class Bit(a: String) extends Gate
case class Not(a: String) extends Gate
case class Wire(a: String) extends Gate
case class Constant(a: Int) extends Gate
case class Or(a: String, b: String) extends Gate
case class And(a: String, b: String) extends Gate
case class LeftShift(a: String, n: Int) extends Gate
case class RightShift(a: String, n: Int) extends Gate

def getGate(str: String) = str match {
    case s"NOT $x" => Not(x)
    case s"1 AND $x" => Bit(x)
    case s"$x OR $y" => Or(x, y)
    case s"$x AND $y" => And(x, y)
    case s"$x LSHIFT $y" => LeftShift(x, y.toInt)
    case s"$x RSHIFT $y" => RightShift(x, y.toInt)
    case other if other.matches("-?\\d+")=> Constant(other.toInt)
    case other => Wire(other)
}

def parseInput(input: List[String]) = input.map(line => {
    val Array(gateString, wire) = line.split(" -> ")
    wire -> getGate(gateString)
}).toMap

def evaluate(wire: String, instructions: Map[String, Gate], cache: MutableMap[String, Int]): Int = {
    return cache.getOrElseUpdate(wire, { 
        instructions(wire) match {
            case Constant(a) => a
            case Wire(s) => evaluate(s, instructions, cache)
            case Bit(a) => evaluate(a, instructions, cache) & 1
            case Not(a) => ~evaluate(a, instructions, cache) & 0xFFFF
            case Or(a, b) => evaluate(a, instructions, cache) | evaluate(b, instructions, cache)
            case And(a, b) => evaluate(a, instructions, cache) & evaluate(b, instructions, cache)
            case LeftShift(a, n) => evaluate(a, instructions, cache) << n
            case RightShift(a, n) => evaluate(a, instructions, cache) >> n
        }
    })
}

def solver(input: List[String]): (Int, Int) = {
    val originalInstructions = parseInput(input)
    val signalA = evaluate("a", originalInstructions, MutableMap.empty)
    
    val modifiedInstructions = originalInstructions + ("b" -> Constant(signalA))
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