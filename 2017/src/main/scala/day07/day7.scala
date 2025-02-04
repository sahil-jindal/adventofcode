package day07

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.Map

case class Node(id: String, children: Seq[String], weight: Int, var treeWeight: Int = -1)

type Tree = Map[String, Node]

def parseInput(input: List[String]): Tree = {
    val tree = Map[String, Node]()
    val pattern =  """([a-z]+) \((\d+)\)( -> (.*))?""".r
    
    for line <- input do {
        line match
            case pattern(id, weight, _, childrenOpt) =>
                val children = Option(childrenOpt).map(_.split(", ").toSeq).getOrElse(Seq.empty)
                tree(id) = Node(id, children, weight.toInt)
            case _ => () // Handle unmatched cases gracefully
    }
    
    return tree
}

def root(tree: Tree): Node = {
    tree.values.find(node => !tree.values.exists(_.children.contains(node.id))).get
}
    
def computeTreeWeights(node: Node, tree: Tree): Int = {
    node.treeWeight = node.weight + node.children.map(childId => computeTreeWeights(tree(childId), tree)).sum
    node.treeWeight
}

def bogusChild(node: Node, tree: Tree): Node = {
    val grouped = node.children.map(tree).groupBy(_.treeWeight).values.toSeq.sortBy(_.size)
    if grouped.size == 1 then null else grouped.head.head
}
    
def fix(node: Node, desiredWeight: Int, tree: Tree): Int = {
    if node.children.size < 2 then throw new NotImplementedError() 

    val bogus = bogusChild(node, tree)
    if bogus == null then desiredWeight - node.treeWeight + node.weight
    else fix(bogus, desiredWeight - node.treeWeight + bogus.treeWeight, tree)
}

def evaluatorOne(tree: Tree): String = root(tree).id

def evaluatorTwo(tree: Tree): Int = {
    val rootNode = root(tree)
    computeTreeWeights(rootNode, tree)
    val bogusChildNode = bogusChild(rootNode, tree)
    val desiredWeight = tree(rootNode.children.find(_ != bogusChildNode.id).get).treeWeight
    fix(bogusChildNode, desiredWeight, tree)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("day07.txt") match
        case Success(lines) => {
            val tree = parseInput(lines)
            println(s"Part One: ${evaluatorOne(tree)}")
            println(s"Part Two: ${evaluatorTwo(tree)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }