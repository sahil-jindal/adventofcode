package day08

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Direction(dy: Int, dx: Int)

case class Point(y: Int, x: Int) {
    def +(dir: Direction) = Point(y + dir.dy, x + dir.dx)
}

val directions = List(Direction(-1, 0), Direction(1, 0), Direction(0, -1), Direction(0, 1))

case class Forest(trees: Map[Point, Int]) {
    type Tree = (Point, Int)
    
    private def treesInDirection(tree: Point, dir: Direction): List[Int] = {
        return Iterator.iterate(tree)(_ + dir).takeWhile(trees.contains).drop(1).map(trees).toList
    }

    private def smallerTrees(tree: Tree, dir: Direction): List[Int] = {
        val (pos, height) = tree
        return treesInDirection(pos, dir).takeWhile(_ < height)
    }

    def isTallest(tree: Tree, dir: Direction): Boolean = {
        val (pos, height) = tree
        return treesInDirection(pos, dir).forall(_ < height)
    }

    def viewDistance(tree: Tree, dir: Direction): Int = {
        if (isTallest(tree, dir)) return treesInDirection(tree(0), dir).size
        return smallerTrees(tree, dir).size + 1
    }
}

def parseInput(input: List[String]): Forest = {
    return Forest((for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
    } yield Point(y, x) -> ch.asDigit).toMap)
}

def evaluatorOne(forest: Forest): Int = {
    return forest.trees.count(tree => directions.exists(forest.isTallest(tree, _)))
}

def evaluatorTwo(forest: Forest): Int = {
    return forest.trees.map(tree => directions.map(forest.viewDistance(tree, _)).product).max
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