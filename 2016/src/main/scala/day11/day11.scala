package day11

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{ListBuffer, Queue, Set, Map => MutableMap}

case class Pair(g: Int, c: Int)
case class State(elevator: Int, elements: Vector[Pair])

def parseInput(input: List[String]): Vector[Pair] = {
    val elementMap = MutableMap.empty[String, Pair].withDefaultValue(Pair(0, 0))

    for ((line, idx) <- input.zipWithIndex) {
        val floor = idx + 1
        
        val generators = raw"([a-z]+) generator".r.findAllMatchIn(line).map(_.group(1))
        val microchips = raw"([a-z]+)-compatible microchip".r.findAllMatchIn(line).map(_.group(1))
        
        for (element <- generators) { elementMap(element) = Pair(floor, elementMap(element).c) }
        for (element <- microchips) { elementMap(element) = Pair(elementMap(element).g, floor) }
    }
    
    return elementMap.values.toVector
}

def applyElements(elements: Vector[Pair], combo: Vector[(Int, Char)], newFloor: Int) = {
    val newElements = elements.zipWithIndex.map { case (it, idx) =>
        val moves = combo.collect { case (i, t) if i == idx => t }
        val newG = if (moves.contains('G')) newFloor else it.g
        val newC = if (moves.contains('M')) newFloor else it.c
        Pair(newG, newC)
    }
        
    newElements.sortBy(t => (t.g, t.c))
}

def isValid(elements: Vector[Pair]): Boolean = {
    return (1 to 4).forall { floor =>
        val generators = elements.exists(_.g == floor)
        val chips = elements.exists(it => it.g != floor && it.c == floor)
        !(generators && chips)
    }
}

def nextStates(current: State): List[State] = {
    val State(currentFloor, elements) = current

    val generators = elements.map(_.g).zipWithIndex.collect {
        case (g, i) if g == currentFloor => (i, 'G')
    }

    val microchips = elements.map(_.c).zipWithIndex.collect {
        case (c, i) if c == currentFloor => (i, 'M')
    }

    val items = generators ++ microchips
    val combinations = items.combinations(1).toList ++ items.combinations(2).toList
    val newFloors = List(currentFloor + 1, currentFloor - 1).filter(f => f >= 1 && f <= 4)

    return (for {
        combo <- combinations
        newFloor <- newFloors
        sortedElements = applyElements(elements, combo, newFloor)
        if isValid(sortedElements)
    } yield State(newFloor, sortedElements))
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

def evaluatorOne(initialElements: Vector[Pair]): Int = {
    return bfs(State(1, initialElements))
}
    
def evaluatorTwo(initialElements: Vector[Pair]): Int = {
    val newElements = initialElements ++ Vector(Pair(1, 1), Pair(1, 1))
    return bfs(State(1, newElements))
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