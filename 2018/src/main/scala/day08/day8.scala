package day08

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Node(children: Seq[Node], metadata: Seq[Int])

def parseInput(input: String) = input.split(" ").map(_.toInt).toSeq
 
def parseNode(input: Seq[Int]): (Node, Seq[Int]) = {
    val numChildren = input.head
    val numMetadata = input(1)
    var remaining = input.drop(2)
 
    val children = for (_ <- 1 to numChildren) yield {
        val (child, nextRem) = parseNode(remaining)
        remaining = nextRem
        child
    }
    
    val (metadata, next) = remaining.splitAt(numMetadata)
    
    return (Node(children, metadata), next)
}
 
def sumMetadata(node: Node): Int = {
    return node.metadata.sum + node.children.map(sumMetadata).sum
}

def calculateNodeValue(node: Node): Int = node match {
    case Node(Nil, metadata) => metadata.sum
    case Node(children, metadata) => metadata.flatMap(i => children.lift(i - 1).map(calculateNodeValue)).sum
}
 
def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day08.txt") match {
        case Success(lines) => {
            val (node, _) = parseNode(parseInput(lines.head))
            println(s"Part One: ${sumMetadata(node)}")
            println(s"Part Two: ${calculateNodeValue(node)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}