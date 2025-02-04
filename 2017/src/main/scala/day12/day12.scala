package day12

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, Set, Stack}

private def parseInput(lines: List[String]): Map[Int, List[Int]] = {
    return lines.map { line =>
        val parts = line.split(" <-> ")
        val key = parts(0).toInt
        val values = parts(1).split(", ").map(_.toInt).toList
        key -> values
    }.toMap
}

def dfs(graph: Map[Int, List[Int]], start: Int): Set[Int] = {
    val visited = Set.empty[Int]
    val stack = Stack(start)

    while (stack.nonEmpty) {
        val node = stack.pop()
        if (!visited.contains(node)) {
            visited.add(node)
            graph.getOrElse(node, List()).foreach { neighbor =>
                if (!visited.contains(neighbor)) {
                    stack.push(neighbor)
                }
            }
        }
    }

    return visited
}

def evaluatorOne(graph: Map[Int, List[Int]]): Int = {
    dfs(graph, 0).size
}

def evaluatorTwo(graph: Map[Int, List[Int]]): Int = {
    var remainingNodes = graph.keys.toSet
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

def hello(): Unit =
    readLinesFromFile("day12.txt") match
        case Success(lines) => {
            val graph = parseInput(lines)
            println(s"Part One: ${evaluatorOne(graph)}")
            println(s"Part Two: ${evaluatorTwo(graph)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }