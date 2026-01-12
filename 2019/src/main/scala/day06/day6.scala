package day06

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

type ChildToParent = Map[String, String]

def parseTree(input: List[String]) = input.collect {
    case s"$parent)$child" => child -> parent
}.toMap

def getAncestors(trees: ChildToParent, node: String): List[String] = {
    return Iterator.iterate(Option(node))(_.flatMap(trees.get)).takeWhile(_.isDefined).drop(1).map(_.get).toList
}

def evaluatorOne(trees: ChildToParent): Int = {
    return trees.keysIterator.map(getAncestors(trees, _).size).sum
}

def evaluatorTwo(trees: ChildToParent): Int = {
    val youAncestor = getAncestors(trees, "YOU").toSet
    val sanAncestor = getAncestors(trees, "SAN").toSet

    // finding the symmetric difference between the two sets
    return ((youAncestor &~ sanAncestor) | (sanAncestor &~ youAncestor)).size
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day06.txt") match {
        case Success(lines) => {
            val trees = parseTree(lines)
            println(s"Part One: ${evaluatorOne(trees)}")
            println(s"Part Two: ${evaluatorTwo(trees)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}