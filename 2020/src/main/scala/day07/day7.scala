package day07

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map, Set}

case class Bags(count: Int, bag: String)
case class Pair(bag: String, children: List[Bags])

val bagPattern = raw"^[a-z]+ [a-z]+ bag".r
val childrenPattern = raw"(\d+) ([a-z]+ [a-z]+ bag)".r

def parseInput(input: List[String]) = input.map(line => {
    val bag = bagPattern.findFirstIn(line).get
    
    val children = childrenPattern.findAllMatchIn(line).map(m => 
        Bags(m.group(1).toInt, m.group(2))
    ).toList
    
    Pair(bag, children)
})

def evaluatorOne(input: List[Pair]): Int = {
    val parentsOf = Map.empty[String, Set[String]]

    for (descr <- input; it <- descr.children) {
        parentsOf.getOrElseUpdate(it.bag, Set.empty) += descr.bag
    }

    def pathsToRoot(bag: String): Set[String] = {
        return parentsOf.getOrElse(bag, Set.empty).flatMap(pathsToRoot) += bag
    }

    return pathsToRoot("shiny gold bag").size - 1
}

def evaluatorTwo(input: List[Pair]): Long = {
    val childrenOf = input.map { case Pair(bag, children) => bag -> children }.toMap

    def countWithChildren(bag: String): Long = {
        return 1 + childrenOf.getOrElse(bag, List.empty).map { 
            case Bags(count, bag) => count * countWithChildren(bag) 
        }.sum
    }

    return countWithChildren("shiny gold bag") - 1
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day07.txt") match {
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