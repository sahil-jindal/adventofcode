package day11

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Set, Queue, Map, ListBuffer}

case class Pair(g: Int, c: Int)
case class State(elevator: Int, elements: List[Pair])

def parseInput(input: List[String]): Map[String, Pair] = {
    val elementMap = Map.empty[String, Pair].withDefaultValue(Pair(0, 0))

    for ((line, idx) <- input.zipWithIndex) {
        val floor = idx + 1
        
        val generators = "([a-z]+) generator".r.findAllMatchIn(line).map(_.group(1))
        val microchips = "([a-z]+)-compatible microchip".r.findAllMatchIn(line).map(_.group(1))
        
        generators.foreach { element => elementMap(element) = Pair(floor, elementMap(element).c) }
        microchips.foreach { element => elementMap(element) = Pair(elementMap(element).g, floor) }
    }
    
    return elementMap
}

def isValid(elements: List[Pair]): Boolean = {
    return (1 to 4).forall { floor =>
        val generators = elements.exists(_.g == floor)
        val chips = elements.exists(it => it.g != floor && it.c == floor)
        !(generators && chips)
    }
}

def nextStates(current: State): List[State] = {
    val State(currentFloor, elements) = current

    val items = elements.zipWithIndex.flatMap { case (it, i) =>
        val list = ListBuffer.empty[(Int, Char)]
        if (it.g == currentFloor) list += ((i, 'G'))
        if (it.c == currentFloor) list += ((i, 'M'))
        list.toList
    }

    val combinations = items.combinations(1).toList ++ items.combinations(2).toList
    val newFloors = List(currentFloor + 1, currentFloor - 1).filter(f => f >= 1 && f <= 4)

    return combinations.flatMap { combo =>
        newFloors.flatMap { newFloor =>
            val newElements = elements.zipWithIndex.map { case (it, idx) =>
                val moves = combo.collect { case (i, t) if i == idx => t }
                val newG = if (moves.contains('G')) newFloor else it.g
                val newC = if (moves.contains('M')) newFloor else it.c
                Pair(newG, newC)
            }
        
            val sortedElements = newElements.sortBy(t => (t.g, t.c))
            val newState = State(newFloor, sortedElements)
            if (isValid(sortedElements)) Some(newState) else None
        }
    }
}

def isGoal(state: State): Boolean = state.elements.forall(it => it.g == 4 && it.c == 4)

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

def solver(input: List[String]): Unit = {
    val initialElements = parseInput(input)
    
    println(s"Part One: ${bfs(State(1, initialElements.values.toList))}")
    
    initialElements("elerium") = Pair(1, 1)
    initialElements("dilithium") = Pair(1, 1)
    
    println(s"Part Two: ${bfs(State(1, initialElements.values.toList))}")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day11.txt") match {
        case Success(lines) => solver(lines)
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}