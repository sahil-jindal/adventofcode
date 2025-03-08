package day13

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map, Set}

class Graph(val people: Set[String], val happiness: Map[(String, String), Int])

def parseInput(lines: List[String]): Graph = {
    val people = Set.empty[String]
    val happiness = Map.empty[(String, String), Int]
    
    lines.foreach(line => {
        val parts = line.split(" ")
        val person1 = parts(0)
        val person2 = parts.last.init // Remove the period
        val value = parts(3).toInt * (if (parts(2) == "gain") 1 else -1)
        happiness((person1, person2)) = value
        people ++= Set(person1, person2)
    })

    return Graph(people, happiness)
}

def findMaximumHappiness(graph: Graph): Int = {
    return graph.people.toSeq.permutations.map { arrangement =>
        val leftRotated = arrangement.tail :+ arrangement.head 
        (arrangement zip leftRotated).foldLeft(0) { case (acc, (a, b)) => 
            acc + graph.happiness((a, b)) + graph.happiness((b, a))
        }
    }.max
}

def addYourself(graph: Graph): Graph = {
    val updatedHappiness = Map(graph.happiness.toSeq*)
    
    graph.people.foreach(guest => {
        updatedHappiness(("You", guest)) = 0
        updatedHappiness((guest, "You")) = 0
    })

    return Graph(graph.people ++ Set("You"), updatedHappiness)
}

def evaluator(input: List[String]): Unit = {
    val graph = parseInput(input)
    var maxHappiness = findMaximumHappiness(graph)
    println(s"Maximum Total Happiness: $maxHappiness")

    val newGraph = addYourself(graph)
    maxHappiness = findMaximumHappiness(newGraph)
    println(s"Maximum Total Happiness (including yourself): $maxHappiness")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day13.txt") match {
        case Success(lines) => evaluator(lines)
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}