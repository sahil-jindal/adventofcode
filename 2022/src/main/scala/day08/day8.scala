package day08

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer

sealed trait Direction(val dy: Int, val dx: Int)
case object Up extends Direction(-1, 0)
case object Down extends Direction(1, 0)
case object Left extends Direction(0, -1)
case object Right extends Direction(0, 1)

case class Tree(height: Int, y: Int, x: Int)

case class Forest(items: List[String], height: Int, width: Int) {
    def trees(): List[Tree] = {
        return (for {
            y <- 0 until height
            x <- 0 until width
        } yield Tree(items(y)(x).toInt, y, x)).toList
    }

    private def treesInDirection(tree: Tree, dir: Direction): List[Tree] = {
        var (y, x) = (tree.y + dir.dy, tree.x + dir.dx)

        val res = ListBuffer.empty[Tree]

        while (y >= 0 && y < height && x >= 0 && x < width) {
            res += Tree(items(y)(x).toInt, y, x)
            y += dir.dy
            x += dir.dx
        }

        return res.toList
    }

    private def smallerTrees(tree: Tree, dir: Direction): List[Tree] = {
        return treesInDirection(tree, dir).takeWhile(_.height < tree.height)
    }

    def isTallest(tree: Tree, dir: Direction): Boolean = {
        return treesInDirection(tree, dir).forall(_.height < tree.height)
    }

    def viewDistance(tree: Tree, dir: Direction): Int = {
        if (isTallest(tree, dir)) return treesInDirection(tree, dir).size
        return smallerTrees(tree, dir).size + 1
    }
}

def parseInput(input: List[String]): Forest = {
    val (width, height) = (input(0).length, input.length)
    return Forest(input, height, width)
}

def evaluatorOne(forest: Forest): Int = {
    return forest.trees().count(tree => 
        forest.isTallest(tree, Left) || forest.isTallest(tree, Right) ||
        forest.isTallest(tree, Up) || forest.isTallest(tree, Down)
    )
}

def evaluatorTwo(forest: Forest): Int = {
    return forest.trees().map(tree => 
        forest.viewDistance(tree, Left) * forest.viewDistance(tree, Right) *
        forest.viewDistance(tree, Up) * forest.viewDistance(tree, Down)
    ).max
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day08.txt") match {
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