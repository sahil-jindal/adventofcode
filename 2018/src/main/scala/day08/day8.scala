package day08

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Node(children: Seq[Node], metadata: Seq[Int])
case class Pair(child: Node, remaining: Seq[Int])

def parseInput(input: String) = input.split(" ").map(_.toInt).toSeq
 
def parseNode(input: Seq[Int]): Pair = {
    var (header, remaining) = input.splitAt(2)
    val Seq(numChildren, numMetadata) = header
 
    val children = for (_ <- 1 to numChildren) yield {
        val Pair(child, nextRem) = parseNode(remaining)
        remaining = nextRem
        child
    }
    
    val (metadata, next) = remaining.splitAt(numMetadata)
    
    return Pair(Node(children, metadata), next)
}
 
def evaluatorOne(node: Node): Int = {
    return node.metadata.sum + node.children.map(evaluatorOne).sum
}

def evaluatorTwo(node: Node): Int = node match {
    case Node(Nil, metadata) => metadata.sum
    case Node(children, metadata) => metadata.flatMap(i => children.lift(i - 1).map(evaluatorTwo)).sum
}
 
def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day08.txt") match {
        case Success(lines) => {
            val node = parseNode(parseInput(lines.head)).child
            println(s"Part One: ${evaluatorOne(node)}")
            println(s"Part Two: ${evaluatorTwo(node)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}