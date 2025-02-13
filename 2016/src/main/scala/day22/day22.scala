package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Node(irow: Int, icol: Int, size: Int, used: Int, goal: Boolean = false) {
    def avail: Int = size - used
}

class Grid(nodes: Array[Array[Node]]) {
    var irowEmpty: Int = 0
    var icolEmpty: Int = 0
    var moves: Int = 0

    for { row <- nodes; node <- row if node.used == 0 } {
        irowEmpty = node.irow
        icolEmpty = node.icol
    }

    val crow: Int = nodes.length
    val ccol: Int = nodes.head.length

    def wall(irow: Int, icol: Int): Boolean = {
        nodes(irow)(icol).used > nodes(irowEmpty)(icolEmpty).size
    }

    def move(drow: Int, dcol: Int): Unit = {
        require(math.abs(drow) + math.abs(dcol) == 1, "Invalid move")

        val irowT = irowEmpty + drow
        val icolT = icolEmpty + dcol

        require(irowT >= 0 && irowT < crow, "Row out of bounds")
        require(icolT >= 0 && icolT < ccol, "Column out of bounds")
        require(nodes(irowT)(icolT).used <= nodes(irowEmpty)(icolEmpty).avail, "Move not possible")

        nodes(irowEmpty)(icolEmpty) = nodes(irowEmpty)(icolEmpty).copy(
            used = nodes(irowT)(icolT).used, 
            goal = nodes(irowT)(icolT).goal
        )

        nodes(irowT)(icolT) = nodes(irowT)(icolT).copy(used = 0, goal = false)
        irowEmpty = irowT
        icolEmpty = icolT
        moves += 1
    }
}


def parseInput(input: List[String]): Array[Array[Node]] = {
    val pattern = "(\\d+)".r
    
    val nodes = input.drop(2).map { line =>
        val parts = pattern.findAllIn(line).map(_.toInt).toArray
        Node(parts(1), parts(0), parts(2), parts(3))
    }.toArray

    val crow = nodes.map(_.irow).max + 1
    val ccol = nodes.map(_.icol).max + 1

    val grid = Array.ofDim[Node](crow, ccol)
    nodes.foreach(n => grid(n.irow)(n.icol) = n)
    grid(0)(ccol - 1) = grid(0)(ccol - 1).copy(goal = true)
    
    return grid
}

def evaluatorOne(nodes: Array[Array[Node]]): Int = {
    return nodes.flatten.combinations(2).count { case Array(nodeA, nodeB) => 
        (nodeA.used > 0 && nodeB.avail > nodeA.used) || (nodeB.used > 0 && nodeA.avail > nodeB.used)
    }
}

def evaluatorTwo(nodes: Array[Array[Node]]): Int = {
    val grid = new Grid(nodes)

    while (grid.irowEmpty != 0) {
        if (!grid.wall(grid.irowEmpty - 1, grid.icolEmpty)) grid.move(-1, 0)
        else grid.move(0, -1)
    }

    while (grid.icolEmpty != grid.ccol - 1) grid.move(0, 1)

    while (!nodes(0)(0).goal) {
        grid.move(1, 0)
        grid.move(0, -1)
        grid.move(0, -1)
        grid.move(-1, 0)
        grid.move(0, 1)
    }

    return grid.moves
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day22.txt") match {
        case Success(lines) => {
            val nodes = parseInput(lines)
            println(s"Part One: ${evaluatorOne(nodes)}")
            println(s"Part Two: ${evaluatorTwo(nodes)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}