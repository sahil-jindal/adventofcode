package day24

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Component(pinA: Int, pinB: Int)
case class Pair(length: Int, strength: Int)

def parseInput(input: List[String]): Set[Component] = {
    return input.map(line => {
        val parts = line.split("/").map(_.toInt)
        Component(parts(0), parts(1))
    }).toSet
}

def strongestBridge(input: List[String], compare: (Pair, Pair) => Int): Int = {
    def fold(pinIn: Int, components: Set[Component]): Pair = {
        var strongest = Pair(0, 0)
        for (component <- components.toList) {
            val pinOut = {
                if (pinIn == component.pinA) then { component.pinB }
                else if (pinIn == component.pinB) then { component.pinA }
                else { -1 }
            }

            if (pinOut != -1) {
                val curr = fold(pinOut, components - component)
                val updated = Pair(curr.length + 1, curr.strength + component.pinA + component.pinB)
                strongest = if (compare(updated, strongest) > 0) then { updated } else { strongest }
            }
        }
        
        return strongest
    }
  
    return fold(0, parseInput(input)).strength
}

def partOne(input: List[String]): Int = strongestBridge(input, (a, b) => a.strength - b.strength)

def partTwo(input: List[String]): Int = strongestBridge(input, (a, b) => {
    if (a.length != b.length) then { a.length - b.length } else { a.strength - b.strength }
})

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day24.txt") match {
        case Success(lines) => {
            println(s"Part One: ${partOne(lines)}")
            println(s"Part Two: ${partTwo(lines)}")
        } 
        case Failure(exception) => {
            println(s"File not found: ${exception.getMessage()}")
        }
    }
}