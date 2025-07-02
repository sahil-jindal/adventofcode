package day11

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{ListBuffer, Queue, Set, Map => MutableMap}

case class Pair(g: Int, c: Int)
case class State(elevator: Int, elements: List[Pair])

type Building = Map[String, Pair]

def parseInput(input: List[String]): Building = {
    val elementMap = MutableMap.empty[String, Pair].withDefaultValue(Pair(0, 0))

    for ((line, idx) <- input.zipWithIndex) {
        val floor = idx + 1
        
        val generators = raw"([a-z]+) generator".r.findAllMatchIn(line).map(_.group(1))
        val microchips = raw"([a-z]+)-compatible microchip".r.findAllMatchIn(line).map(_.group(1))
        
        for (element <- generators) { elementMap(element) = Pair(floor, elementMap(element).c) }
        for (element <- microchips) { elementMap(element) = Pair(elementMap(element).g, floor) }
    }
    
    return elementMap.toMap
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

def isGoal(state: State) = state.elements.forall(it => it.g == 4 && it.c == 4)

def bfs(initial: State): Int = {
    val queue = Queue((initial, 0))
    val visited = Set(initial)

    while (queue.nonEmpty) {
        val (current, steps) = queue.dequeue()
        
        if (isGoal(current)) return steps
        
        for (nextState <- nextStates(current)) {
            if (!visited.contains(nextState)) {
                visited.add(nextState)
                queue.enqueue((nextState, steps + 1))
            }
        }
    }
    
    throw new Exception("No solution found!")
}

def evaluatorOne(initialElements: Building): Int = {
    return bfs(State(1, initialElements.values.toList))
}
    
def evaluatorTwo(initialElements: Building): Int = {
    val newElements = initialElements ++ Map("elerium" -> Pair(1, 1), "dilithium" -> Pair(1, 1))
    return bfs(State(1, newElements.values.toList))
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day11.txt") match {
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