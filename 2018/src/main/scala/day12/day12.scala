package day12

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.control.Breaks._

case class State(left: Long, pots: IndexedSeq[Boolean])

type Input = (state: State, rules: Map[IndexedSeq[Boolean], Boolean])

def checkPot: PartialFunction[Char, Boolean] = {
    case '#' => true
    case '.' => false
}

def parseInput(input: List[String]): Input = {
    val pots = input.head.stripPrefix("initial state: ").collect(checkPot)
    
    val rules = input.drop(2).collect {
        case s"$key => $value" => key.collect(checkPot) -> checkPot(value.head)
    }.toMap
    
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

def iterate(input: Input, iterations: Long): Long = {
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

def evaluatorOne(input: Input): Long = iterate(input, 20)
def evaluatorTwo(input: Input): Long = iterate(input, 50000000000L)

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