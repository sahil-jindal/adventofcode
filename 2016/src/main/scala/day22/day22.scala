package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Node(y: Int, x: Int, size: Int, used: Int, goal: Boolean = false) {
    val avail: Int = size - used
}

class Grid(nodes: Array[Array[Node]]) {
    var yEmpty = 0
    var xEmpty = 0
    var moves = 0

    for { row <- nodes; node <- row if node.used == 0 } {
        yEmpty = node.y
        xEmpty = node.x
    }

    val height = nodes.length
    val width = nodes.head.length

    def wall(y: Int, x: Int): Boolean = {
        nodes(y)(x).used > nodes(yEmpty)(xEmpty).size
    }

    def move(dy: Int, dx: Int): Unit = {
        require(dy.abs + dx.abs == 1, "Invalid move")

        val yT = yEmpty + dy
        val xT = xEmpty + dx

        require(yT >= 0 && yT < height, "Row out of bounds")
        require(xT >= 0 && xT < width, "Column out of bounds")
        require(nodes(yT)(xT).used <= nodes(yEmpty)(xEmpty).avail, "Move not possible")

        nodes(yEmpty)(xEmpty) = nodes(yEmpty)(xEmpty).copy(
            used = nodes(yT)(xT).used, 
            goal = nodes(yT)(xT).goal
        )

        nodes(yT)(xT) = nodes(yT)(xT).copy(used = 0, goal = false)
        yEmpty = yT
        xEmpty = xT
        moves += 1
    }
}


def parseInput(input: List[String]): Array[Array[Node]] = {
    val nodes = input.drop(2).map(line => {
        val Seq(y, x, size, used) = "(\\d+)".r.findAllIn(line).map(_.toInt).toSeq
        Node(y, x, size, used)
    })

    val height = nodes.map(_.y).max + 1
    val width = nodes.map(_.x).max + 1

    val grid = Array.ofDim[Node](height, width)

    nodes.foreach(n => grid(n.y)(n.x) = n)
    
    grid(0)(width - 1) = grid(0)(width - 1).copy(goal = true)
    
    return grid
}

def evaluatorOne(nodes: Array[Array[Node]]): Int = {
    return nodes.flatten.combinations(2).count { case Array(nodeA, nodeB) => 
        (nodeA.used > 0 && nodeB.avail > nodeA.used) || (nodeB.used > 0 && nodeA.avail > nodeB.used)
    }
}

def evaluatorTwo(nodes: Array[Array[Node]]): Int = {
    val grid = new Grid(nodes)

    while (grid.yEmpty != 0) {
        if (!grid.wall(grid.yEmpty - 1, grid.xEmpty)) grid.move(-1, 0)
        else grid.move(0, -1)
    }

    while (grid.xEmpty != grid.width - 1) grid.move(0, 1)

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