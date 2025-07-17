package day07

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map, Set, PriorityQueue}

case class Worker(var task: Option[Char], var remaining: Int)

val parseRegex = raw"Step (\w) must be finished before step (\w) can begin.".r

def parseInput(input: List[String]) = {
    // Build reverse dependency graph and in-degree
    val reverseGraph = Map.empty[Char, Set[Char]]
    val inDegree = Map.empty[Char, Int].withDefaultValue(0)

    for (line <- input) {
        val List(from, to) = parseRegex.findFirstMatchIn(line).get.subgroups.map(_.head)

        reverseGraph.getOrElseUpdate(from, Set.empty) += to
        reverseGraph.getOrElseUpdate(to, Set.empty)

        inDegree(to) += 1
        inDegree.getOrElseUpdate(from, 0)
    }

    (reverseGraph.toMap, inDegree)
}

def evaluatorOne(input: List[String]): String = {
    val (reverseGraph, inDegree) = parseInput(input)

    val available = PriorityQueue.empty(using Ordering.Char.reverse)

    for ((node, deg) <- inDegree if deg == 0) available.enqueue(node)

    val result = new StringBuilder

    while (available.nonEmpty) {
        val current = available.dequeue()
        result.append(current)

        for (neighbor <- reverseGraph(current)) {
            inDegree(neighbor) -= 1
            if (inDegree(neighbor) == 0) {
                available.enqueue(neighbor)
            }
        }
    }

    return result.toString()
}

def evaluatorTwo(input: List[String]): Int = {
    val (reverseGraph, inDegree) = parseInput(input)

    val available = PriorityQueue.empty(using Ordering.Char.reverse)

    for ((node, deg) <- inDegree if deg == 0) available.enqueue(node)

    val workers = Array.fill(5)(Worker(None, 0))

    var timeElapsed = 0

    while (available.nonEmpty || workers.exists(_.task.isDefined)) {
        for (worker <- workers if worker.task.isEmpty && available.nonEmpty) {
            val task = available.dequeue()
            worker.task = Some(task)
            worker.remaining = 60 + (task - 'A' + 1)
        }

        timeElapsed += 1

        for (worker <- workers if worker.task.isDefined) {
            worker.remaining -= 1

            if (worker.remaining == 0) {
                val finished = worker.task.get

                for (neighbor <- reverseGraph(finished)) {
                    inDegree(neighbor) -= 1
                    if (inDegree(neighbor) == 0) {
                        available.enqueue(neighbor)
                    }
                }

                worker.task = None
            }
        }
    }

    return timeElapsed
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day07.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}