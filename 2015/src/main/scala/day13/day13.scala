package day13

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

case class Graph(happiness: Map[(String, String), Int]) {
    val people = happiness.keySet.flatMap(List(_,_))
}

val gainRegex = raw"(\w+) would gain (\d+) happiness units by sitting next to (\w+).".r
val loseRegex = raw"(\w+) would lose (\d+) happiness units by sitting next to (\w+).".r

def parseInput(input: List[String]): Graph = {
    val happiness = input.collect {
        case gainRegex(person1, b, person2) => (person1, person2) -> b.toInt
        case loseRegex(person1, b, person2) => (person1, person2) -> -b.toInt
    }.toMap

    return Graph(happiness)
}

def findMaximumHappiness(graph: Graph): Int = {
    return graph.people.toSeq.permutations.map { arrangement =>
        val leftRotated = arrangement.tail :+ arrangement.head 
        (arrangement zip leftRotated).map { case (a, b) => 
            graph.happiness((a, b)) + graph.happiness((b, a))
        }.sum
    }.max
}

def addYourself(graph: Graph): Graph = {
    val newHappiness = graph.people.flatMap(p => List(("You", p), (p, "You"))).map(_ -> 0).toMap
    return Graph(graph.happiness ++ newHappiness)
}

def evaluatorOne(graph: Graph): Int = findMaximumHappiness(graph)
def evaluatorTwo(graph: Graph): Int = findMaximumHappiness(addYourself(graph))

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day13.txt") match {
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