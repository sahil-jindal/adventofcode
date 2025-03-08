package day07

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

 def parseInstructions(input: List[String]): Map[String, Vector[String]] = {
    val instructions = Map.empty[String, Vector[String]]
    
    input.foreach(line => {
        val Array(expr, wire) = line.split(" -> ")
        instructions(wire) = expr.split(" ").toVector
    })

    return instructions
}

def evaluate(wire: String, instructions: Map[String, Vector[String]], cache: Map[String, Int]): Int = {
    if (cache.contains(wire)) return cache(wire)
    if (wire.forall(_.isDigit)) return wire.toInt

    val value = instructions(wire) match {
        case Vector(a, "AND", b) => evaluate(a, instructions, cache) & evaluate(b, instructions, cache)
        case Vector(a, "OR", b) => evaluate(a, instructions, cache) | evaluate(b, instructions, cache)
        case Vector(a, "LSHIFT", n) => evaluate(a, instructions, cache) << n.toInt
        case Vector(a, "RSHIFT", n) => evaluate(a, instructions, cache) >> n.toInt
        case Vector("NOT", a) => ~evaluate(a, instructions, cache) & 0xFFFF
        case s => evaluate(s.mkString, instructions, cache) // Direct assignment
    }

    cache(wire) = value
    value
}

def evaluator(input: List[String]) = {
    val originalInstructions = parseInstructions(input)
    val signalA = evaluate("a", originalInstructions, Map.empty[String, Int])
    println(s"Signal to wire 'a' (Part One): $signalA")

    val modifiedInstructions = originalInstructions + ("b" -> Vector(signalA.toString))
    val newSignalA = evaluate("a", modifiedInstructions, Map.empty[String, Int])
    println(s"Signal to wire 'a' (Part Two): $newSignalA")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day07.txt") match {
        case Success(lines) => evaluator(lines)
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}