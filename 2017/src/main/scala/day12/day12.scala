package day12

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Stack, Set => MutableSet}

def parseInput(input: List[String]) = input.collect {
    case s"$first <-> $second" => first -> second.split(", ").toList
}.toMap

def dfs(graph: Map[String, List[String]], start: String): Set[String] = {
    val visited = MutableSet.empty[String]
    val stack = Stack(start)

    while (stack.nonEmpty) {
        val node = stack.pop()
        if (visited.add(node) && graph.contains(node)) {
            stack.pushAll(graph(node).filterNot(visited.contains))
        }
    }

    return visited.toSet
}

def evaluatorOne(graph: Map[String, List[String]]): Int = dfs(graph, "0").size

def evaluatorTwo(graph: Map[String, List[String]]): Int = {
    val remainingNodes = MutableSet.from(graph.keySet)
    var groups = 0

    while (remainingNodes.nonEmpty) {
        val start = remainingNodes.head
        val group = dfs(graph, start)
        remainingNodes --= group
        groups += 1
    }

    return groups
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day12.txt") match {
        case Success(lines) => {
            val graph = parseInput(lines)
            println(s"Part One: ${evaluatorOne(graph)}")
            println(s"Part Two: ${evaluatorTwo(graph)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}