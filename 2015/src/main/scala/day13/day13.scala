package day13

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

case class Graph(people: Set[String], happiness: Map[(String, String), Int])

val parseRegex = raw"(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)".r

def parseInput(input: List[String]): Graph = {
    val people = MutableSet.empty[String]
    val happiness = MutableMap.empty[(String, String), Int]
    
    for (line <- input) {
        val List(person1, a, b, person2) = parseRegex.findFirstMatchIn(line).get.subgroups
        val value = b.toInt * (if (a == "gain") 1 else -1)
        happiness((person1, person2)) = value
        people ++= Set(person1, person2)
    }

    return Graph(people.toSet, happiness.toMap)
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
    val updatedHappiness = MutableMap.from(graph.happiness)
    
    for (guest <- graph.people) {
        updatedHappiness(("You", guest)) = 0
        updatedHappiness((guest, "You")) = 0
    }

    return Graph(graph.people.incl("You"), updatedHappiness.toMap)
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