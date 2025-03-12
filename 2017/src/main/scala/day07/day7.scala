package day07

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

val pattern =  """(\w+) \((\d+)\)(?: -> ([\w, ]+))?""".r

case class Node(id: String, children: Seq[String], weight: Int, var treeWeight: Int = -1)

type Tree = Map[String, Node]

def parseInput(input: List[String]): Tree = {    
    return input.map(line => {
        val List(id, weight, childrenOpt) = pattern.findFirstMatchIn(line).get.subgroups
        val children = Option(childrenOpt).map(_.split(", ").toSeq).getOrElse(Seq.empty)
        id -> Node(id, children, weight.toInt)
    }).toMap
}

def root(tree: Tree): Node = {
    val allNodes = tree.keySet
    val allChildren = tree.values.flatMap(_.children).toSet
    return tree(allNodes.diff(allChildren).head)
}
    
def computeTreeWeights(node: Node, tree: Tree): Int = {
    node.treeWeight = node.weight + node.children.map(childId => computeTreeWeights(tree(childId), tree)).sum
    return node.treeWeight
}

def bogusChild(node: Node, tree: Tree): Node = {
    val grouped = node.children.map(tree).groupBy(_.treeWeight).values.toSeq.sortBy(_.size)
    return if grouped.size == 1 then null else grouped.head.head
}
    
def fix(node: Node, desiredWeight: Int, tree: Tree): Int = {
    val bogus = bogusChild(node, tree)
    if bogus == null then return desiredWeight - node.treeWeight + node.weight
    return fix(bogus, desiredWeight - node.treeWeight + bogus.treeWeight, tree)
}

def solver(tree: Tree): (String, Int) = {
    val rootNode = root(tree)
    computeTreeWeights(rootNode, tree)
    val bogusChildNode = bogusChild(rootNode, tree)
    val desiredWeight = tree(rootNode.children.find(_ != bogusChildNode.id).get).treeWeight
    val correctWeight = fix(bogusChildNode, desiredWeight, tree)
    return (rootNode.id, correctWeight)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day07.txt") match {
        case Success(lines) => {
            val (root, correctWeight) = solver(parseInput(lines))
            println(s"Part One: $root")
            println(s"Part Two: $correctWeight")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}