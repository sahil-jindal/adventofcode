package day07

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Queue
import scala.util.control.Breaks._

extension (arr: Array[Int]) {
    def swap(a: Int, b: Int): Unit = {
        val temp = arr(a)
        arr(a) = arr(b)
        arr(b) = temp
    }
}

case class TreeNode(weight: Int, children: List[String])

case class Node(
    var hasParent: Boolean = false,
    var parent: String = "",
    var children: Int = 0,
    var processed: Int = 0,
    var weight: Int = 0,
    var total: Int = 0,
    val subWeights: Array[Int] = Array(0, 0),
    val subTotals: Array[Int] = Array(0, 0),
)

val pattern =  raw"(\w+) \((\d+)\)(?: -> ([\w, ]+))?".r

def parseInput(input: List[String]) = input.collect {
    case pattern(id, weight, childrenOpt) => {
        val children = Option(childrenOpt).map(_.split(", ").toList).getOrElse(List.empty)
        id -> TreeNode(weight.toInt, children)
    }
}.toMap

def solver(pairs: Map[String, TreeNode]): (String, Int) = {
    val nodes = pairs.keySet.map(_ -> Node()).toMap

    for ((id, TreeNode(weight, children)) <- pairs) {
        nodes(id).weight = weight
        nodes(id).total = weight
        nodes(id).children = children.size

        for (edge <- children) {
            nodes(edge).parent = id
            nodes(edge).hasParent = true
        }
    }

    // The root is the only node without a parent. Start from any node, 
    // and walk up the tree until finding the root.
    var partOne = pairs.keySet.head

    while (nodes(partOne).hasParent) {
        partOne = nodes(partOne).parent
    }

    val todo = Queue.from(nodes.collect { 
        case (id, node) if node.children == 0 => id 
    })
    
    var partTwo = 0

    breakable {
        while (todo.nonEmpty) {
            val index = todo.dequeue()
            val Node(_, parent, _, _, weight, total, _, _) = nodes(index)
            val node = nodes(parent)

            if (node.processed < 2) {
                 // Fill out the first two children in any order.
                node.subWeights(node.processed) = weight
                node.subTotals(node.processed) = total
            } else {
                // Representing the balanced nodes as `b` and the unbalanced node as `u`,
                // there are 4 possibilities:
                // b3 + [b1 b2] => [b2 b1] Swap, keep accumulating
                // b3 + [b1 u2] => [u2 b1] Swap, unbalanced node identified
                // u3 + [b1 b2] -> [u3 b2] Overwrite, unbalanced node identified
                // b3 + [u1 b2] => [u1 b2] Do nothing, unbalanced node identified
                // The unbalanced node will always be first (if it exists).
                if (node.subTotals(0) == total) {
                    node.subWeights.swap(0, 1)
                    node.subTotals.swap(0, 1)
                } else if (node.subTotals(1) != total) {
                    node.subWeights(0) = weight
                    node.subTotals(0) = total
                }
            
                // If the unbalanced node was identified, it is now first, 
                // and we can short-circuit summing the weights of the rest of the tree.
                val Array(x, y) = node.subTotals

                if (x != y) {
                    partTwo = node.subWeights(0) - x + y
                    break()
                }
            }

            node.total += total
            node.processed += 1

            // If we've processed all children then add to the queue and check balance.
            if (node.processed == node.children) {
                todo.enqueue(parent)
            }
        }
    }

    return (partOne, partTwo)
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