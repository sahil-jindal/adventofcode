package day12

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.control.Breaks._

case class State(left: Long, pots: String)

def parseInput(input: List[String]): (State, Map[String, String]) = {
    val state = State(0, input.head.stripPrefix("initial state: "))
    
    val rules = input.drop(2).map(line => {
        val Array(key, value) = line.split(" => ")
        key -> value
    }).toMap
    
    return (state, rules)
}

def step(state: State, rules: Map[String, String]): State = {
    val pots = "....." + state.pots + "....."
    
    val newPots = pots.sliding(5).map(pot => rules.getOrElse(pot, ".")).mkString

    val firstFlower = newPots.indexOf('#')
    val newLeft = firstFlower + state.left - 3

    val trimmedPots = newPots.drop(firstFlower).reverse.dropWhile(_ == '.').reverse
    
    return State(newLeft, trimmedPots)
}

def iterate(input: List[String], iterations: Long): Long = {
    var (state, rules) = parseInput(input)
    var remainingIterations = iterations
    var dLeftPos = 0L

    breakable {
        while (remainingIterations > 0) {
            val prevState = state
            state = step(state, rules)
            remainingIterations -= 1
            dLeftPos = state.left - prevState.left
            
            if (state.pots == prevState.pots) {
                state = state.copy(left = state.left + remainingIterations * dLeftPos)
                break()
            }
        }
    }
    
    return state.pots.zipWithIndex.collect { case ('#', i) => i + state.left }.sum
}

def evaluatorOne(input: List[String]): Long = iterate(input, 20)
def evaluatorTwo(input: List[String]): Long = iterate(input, 50000000000L)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day12.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}