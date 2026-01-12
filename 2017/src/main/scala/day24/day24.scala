package day24

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Component(pinA: Int, pinB: Int)
case class Pair(length: Int, strength: Int)

val partOneOrdering = Ordering.by[Pair, Int](_.strength)
val partTwoOrdering = Ordering.by[Pair, Int](_.length) orElse Ordering.by(_.strength)

def parseInput(input: List[String]) = input.collect {
    case s"$pinA/$pinB" => Component(pinA.toInt, pinB.toInt)
}.toSet

def strongestBridge(input: Set[Component], compare: (Pair, Pair) => Int): Int = {
    def fold(pinIn: Int, components: Set[Component]): Pair = {
        var strongest = Pair(0, 0)

        for (component <- components.toList) {
            val pinOut = {
                if (pinIn == component.pinA) { component.pinB }
                else if (pinIn == component.pinB) { component.pinA }
                else { -1 }
            }

            if (pinOut != -1) {
                val curr = fold(pinOut, components - component)
                val updated = Pair(curr.length + 1, curr.strength + component.pinA + component.pinB)
                strongest = if (compare(updated, strongest) > 0) { updated } else { strongest }
            }
        }
        
        return strongest
    }
  
    return fold(0, input).strength
}

def evaluatorOne(input: Set[Component]): Int = strongestBridge(input, partOneOrdering.compare)
def evaluatorTwo(input: Set[Component]): Int = strongestBridge(input, partTwoOrdering.compare)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day24.txt") match {
        case Success(lines) => {
            val input = parseInput(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        } 
        case Failure(exception) => {
            println(s"File not found: ${exception.getMessage()}")
        }
    }
}