package day12

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.control.Breaks._

case class State(left: Long, pots: IndexedSeq[Boolean])

type Pair = (state: State, rules: Map[IndexedSeq[Boolean], Boolean])

def checkPots(input: String) = input.collect {
    case '#' => true
    case '.' => false
}

def parseInput(input: List[String]): Pair = {
    val pots = checkPots(input.head.stripPrefix("initial state: "))
    
    val rules = input.drop(2).map(line => {
        val Array(key, value) = line.split(" => ").map(checkPots)
        key -> value.head
    }).toMap
    
    return (State(0, pots), rules)
}

def step(state: State, rules: Map[IndexedSeq[Boolean], Boolean]): State = {
    val margin = IndexedSeq.fill(5)(false)
    val pots = margin ++ state.pots ++ margin
    
    val newPots = pots.sliding(5).map(pot => rules.getOrElse(pot, false)).toIndexedSeq

    val firstFlower = newPots.indexOf(true)
    val lastFlower = newPots.lastIndexOf(true)
    
    return State(firstFlower + state.left - 3, newPots.slice(firstFlower, lastFlower + 1))
}

def iterate(input: Pair, iterations: Long): Long = {
    var (state, rules) = input
    var remainingIterations = iterations

    breakable {
        while (remainingIterations > 0) {
            val prevState = state
            state = step(state, rules)
            remainingIterations -= 1
            val dLeftPos = state.left - prevState.left
            
            if (state.pots == prevState.pots) {
                state = state.copy(left = state.left + remainingIterations * dLeftPos)
                break()
            }
        }
    }
    
    return state.pots.zipWithIndex.collect { case (true, i) => i + state.left }.sum
}

def evaluatorOne(input: Pair): Long = iterate(input, 20)
def evaluatorTwo(input: Pair): Long = iterate(input, 50000000000L)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day12.txt") match {
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