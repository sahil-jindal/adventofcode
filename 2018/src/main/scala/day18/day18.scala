package day18

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map
import scala.util.control.Breaks._

sealed abstract class Content(val ch: Char)
case object Yard extends Content('#')
case object Tree extends Content('|')
case object Open extends Content('.')

type Grid = IndexedSeq[IndexedSeq[Content]]

def parseInput(input: List[String]): Grid = {
    return input.map(_.collect {
        case '#' => Yard
        case '|' => Tree
        case '.' => Open
    }).toIndexedSeq
}

def haloPaddingGrid(input: Grid): Grid = {
    val (h, w) = (input.length, input(0).length)
    val grid = Array.fill[Content](h + 2, w + 2)(Open)

    for (y <- 0 until h; x <- 0 until w) {
        grid(y + 1)(x + 1) = input(y)(x)
    }

    return grid.map(_.toIndexedSeq).toIndexedSeq
}

def contentCondition(grid: Grid, y: Int, x: Int): Content = {
    val neighbours = (for {
        dy <- -1 to 1
        dx <- -1 to 1
        if dy != 0 || dx != 0
    } yield grid(y + dy)(x + dx))

    val freq = neighbours.groupMapReduce(identity)(_ => 1)(_ + _)

    val (tree, lumberyard) = (freq.getOrElse(Tree, 0), freq.getOrElse(Yard, 0))

    return grid(y)(x) match {
        case Yard if lumberyard >= 1 && tree >= 1 => Yard
        case Tree if lumberyard >= 3 => Yard
        case Open if tree >= 3 => Tree
        case Yard => Open
        case c => c
    }
}

def step(grid: Grid): Grid = {
    val (r, c) = (grid.length, grid(0).length)

    val newGrid = haloPaddingGrid(grid)

    return IndexedSeq.tabulate(r, c) { case (y, x) => 
        contentCondition(newGrid, y + 1, x + 1)
    }
}

def iterate(input: Grid, lim: Int): Int = {
    val seen = Map.empty[String, Int]
    var mtx = input
    
    breakable {
        for (t <- 0 until lim) {
            val hash = mtx.flatten.map(_.ch).mkString
            
            if (seen.contains(hash)) {
                // Found a cycle, calculate the final state
                val cycleLength = t - seen(hash)
                val remainingCycles = (lim - t) % cycleLength
            
                // Fast-forward by running only the remaining cycles
                for (_ <- 0 until remainingCycles) {
                    mtx = step(mtx)
                }

                break()
            }
            
            seen(hash) = t
            mtx = step(mtx)
        }
    }

    val frequencies = mtx.flatten.groupMapReduce(identity)(_ => 1)(_ + _)

    return frequencies(Tree) * frequencies(Yard)
}

def evaluatorOne(input: Grid): Int = iterate(input, 10)
def evaluatorTwo(input: Grid): Int = iterate(input, 1000000000)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day18.txt") match {
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