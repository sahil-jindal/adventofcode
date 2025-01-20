package dayseven

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

 def parseInstructions(input: List[String]): Map[String, String] = {
    val instructions = Map[String, String]()
    
    for (line <- input) {
        val parts = line.split(" -> ")
        val wire = parts(1).trim // Destination wire
        val expr = parts(0).trim // Expression or signal
        instructions(wire) = expr
    }

    instructions
}

def evaluate(wire: String, instructions: Map[String, String], cache: Map[String, Int]): Int = {
    // If the wire is already cached, return its value
    if (cache.contains(wire)) return cache(wire)

    // If the wire is a numeric value, return it directly
    if (wire.forall(_.isDigit)) return wire.toInt

    // Fetch the operation for this wire
    val operation = instructions(wire)

    // Evaluate the result based on the operation
    val value = operation match {
        case s if s.contains("AND") =>
            val Array(a, b) = s.split(" AND ")
            evaluate(a, instructions, cache) & evaluate(b, instructions, cache)
        case s if s.contains("OR") =>
            val Array(a, b) = s.split(" OR ")
            evaluate(a, instructions, cache) | evaluate(b, instructions, cache)
        case s if s.contains("LSHIFT") =>
            val Array(a, n) = s.split(" LSHIFT ")
            evaluate(a, instructions, cache) << n.toInt
        case s if s.contains("RSHIFT") =>
            val Array(a, n) = s.split(" RSHIFT ")
            evaluate(a, instructions, cache) >> n.toInt
        case s if s.startsWith("NOT") =>
            val a = s.stripPrefix("NOT ")
            ~evaluate(a, instructions, cache) & 0xFFFF
        case s =>
            evaluate(s, instructions, cache) // Direct assignment
    }

    // Cache the computed value and return it
    cache(wire) = value
    value
}

def evaluator(input: List[String]) = {
    val originalInstructions = parseInstructions(input)

    // Part One: Compute the signal for wire "a"
    val cachePartOne = Map[String, Int]()
    val signalA = evaluate("a", originalInstructions, cachePartOne)
    println(s"Signal to wire 'a' (Part One): $signalA")

    // Part Two: Override wire "b" and recompute wire "a"
    val modifiedInstructions = originalInstructions + ("b" -> signalA.toString)
    val cachePartTwo = Map[String, Int]()
    val newSignalA = evaluate("a", modifiedInstructions, cachePartTwo)
    println(s"Signal to wire 'a' (Part Two): $newSignalA")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("dayseven.txt") match
        case Success(lines) => {
            evaluator(lines)
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }