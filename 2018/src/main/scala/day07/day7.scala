package day07

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map, Set}

def parseInput(input: List[String]): Map[Char, Set[Char]] = {
    val dict = Map.empty[Char, Set[Char]]

    for (line <- input) {
        val parts = line.split(" ")
        val part = parts(7).head
        val partDependsOn = parts(1).head
        dict.getOrElseUpdate(part, Set.empty) += partDependsOn
        dict.getOrElseUpdate(partDependsOn, Set.empty)
    }

    return dict
}

def evaluatorOne(input: List[String]): String = {
    val sb = new StringBuilder
    val graph = parseInput(input)

    while (graph.nonEmpty) {
        val minKey = graph.keys.filter(k => graph(k).isEmpty).min
        sb.append(minKey)
        graph.remove(minKey)
        graph.keys.foreach(k => graph(k) -= minKey)
    }

    return sb.toString
}

def evaluatorTwo(input: List[String]): Int = {
    var time = 0
    val graph = parseInput(input)

    val works = Array.fill(5)(0)
    val items = Array.fill(5)(' ')

    while (graph.nonEmpty || works.exists(_ > 0)) {
        for (i <- works.indices if graph.nonEmpty && works(i) == 0) {
            val found = graph.keys.filter(k => graph(k).isEmpty).minOption
            
            if (found.isDefined) {
                val minKey = found.get
                works(i) = 60 + minKey - 'A' + 1
                items(i) = minKey
                graph.remove(minKey)
            }
        }

        time += 1

        for (i <- works.indices) {
            if (works(i) > 0) {
                works(i) -= 1
                if (works(i) == 0) {
                    graph.keys.foreach(k => graph(k) -= items(i))
                }
            }
        }
    }

    return time
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