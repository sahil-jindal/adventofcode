package day06

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer

type ChildToParent = Map[String, String]

def parseTree(input: List[String]) = input.map(line => {
    val Array(parent, child) = line.split("\\)", 2)
    child -> parent
}).toMap

def getAncestors(trees: ChildToParent, node: String): List[String] = {
    val res = ListBuffer.empty[String]
    var parent = trees.get(node)

    while (parent.isDefined) {
        val value = parent.get
        res += value
        parent = trees.get(value)
    }

    return res.toList
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