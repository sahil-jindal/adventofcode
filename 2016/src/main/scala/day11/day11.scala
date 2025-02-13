package day11

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Set, Queue, Map}
import scala.util.boundary, boundary.break

case class State(elevator: Int, elements: List[(Int, Int)]) {
    override def toString: String = s"State($elevator, ${elements.mkString("[", ", ", "]")})"
}

def parseInput(input: List[String]) = {
    val elementMap = Map[String, (Int, Int)]().withDefaultValue((0, 0))

    for ((line, idx) <- input.zipWithIndex) {
        val floor = idx + 1
        
        val generators = "([a-z]+) generator".r.findAllMatchIn(line).map(_.group(1)).toList
        val microchips = "([a-z]+)-compatible microchip".r.findAllMatchIn(line).map(_.group(1)).toList
        
        generators.foreach { element =>
            elementMap(element) = (floor, elementMap(element)._2)
        }
        
        microchips.foreach { element =>
            elementMap(element) = (elementMap(element)._1, floor)
        }
    }
    
    elementMap
}

def isValid(elements: List[(Int, Int)]): Boolean = {
    boundary {
        for (floor <- 1 to 4) {
            val generators = elements.exists { case (a, _) => a == floor }
            val chips = elements.exists { case (a, b) => a != floor && b == floor }
            if generators && chips then {
                break(false)
            }
        }
    }

    true
}

def nextStates(current: State): List[State] = {
    val currentFloor = current.elevator
    val elements = current.elements

    val items = elements.zipWithIndex.flatMap { case ((g, c), i) =>
        var list = List.empty[(Int, Char)]
        if (g == currentFloor) list = list :+ (i, 'G')
        if (c == currentFloor) list = list :+ (i, 'M')
        list
    }

    val combinations = items.combinations(1).toList ++ items.combinations(2).toList
    val newFloors = List(currentFloor + 1, currentFloor - 1).filter(f => f >= 1 && f <= 4)

    return combinations.flatMap { combo =>
        newFloors.flatMap { newFloor =>
            val newElements = elements.zipWithIndex.map { case ((g, c), idx) =>
                val moves = combo.collect { case (i, t) if i == idx => t }
                val newG = if (moves.contains('G')) newFloor else g
                val newC = if (moves.contains('M')) newFloor else c
                (newG, newC)
            }
        
            val sortedElements = newElements.sortBy(t => (t._1, t._2))
            val newState = State(newFloor, sortedElements)
            if (isValid(sortedElements)) Some(newState) else None
        }
    }
}

def isGoal(state: State): Boolean = {
    return state.elements.forall { case (g, c) => g == 4 && c == 4 }
}

def bfs(initial: State): Int = {
    val queue = Queue((initial, 0))
    val visited = Set(initial)

    while (queue.nonEmpty) {
        val (current, steps) = queue.dequeue()
        
        if (isGoal(current)) return steps
        
        nextStates(current).foreach { nextState =>
            if (!visited.contains(nextState)) {
                visited.add(nextState)
                queue.enqueue((nextState, steps + 1))
            }
        }
    }
    
    return -1
}

def evaluatorOne(input: List[String]): Int = {
    val initialElements = parseInput(input)
    return bfs(State(1, initialElements.values.toList))
}

def evaluatorTwo(input: List[String]): Int = {
    val initialElements = parseInput(input)

    initialElements("elerium") = (1, 1)
    initialElements("dilithium") = (1, 1)
    
    return bfs(State(1, initialElements.values.toList))
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day11.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}