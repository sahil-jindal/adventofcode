package day18

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

def parseInput(input: List[String]) = input.map(_.toList)

def step(mtx: List[List[Char]]): List[List[Char]] = {
    val (height, width) = (mtx.length, mtx(0).length)
    val res = Array.ofDim[Char](height, width)

    for (y <- 0 until height; x <- 0 until width) {
        var (tree, lumberyard, empty) = (0, 0, 0)
    
        for (dy <- -1 to 1; dx <- -1 to 1; if dy != 0 || dx != 0) {
            val (xT, yT) = (x + dx, y + dy)
            if (xT >= 0 && xT < width && yT >= 0 && yT < height) {
                mtx(yT)(xT) match {
                    case '#' => lumberyard += 1
                    case '|' => tree += 1
                    case '.' => empty += 1
                    case _   =>
                }
            }
        }

        res(y)(x) = mtx(y)(x) match {
            case '#' if lumberyard >= 1 && tree >= 1 => '#'
            case '|' if lumberyard >= 3 => '#'
            case '.' if tree >= 3 => '|'
            case '#' => '.'
            case c => c
        }
    }

    return res.map(_.toList).toList
}

def iterate(input: List[List[Char]], lim: Int): Int = {
    val seen = Map.empty[String, Int]
    var mtx = input
    
    var t = 0
    
    while (t < lim) {
        val hash = mtx.flatten.mkString
        
        if (seen.contains(hash)) {
            val loopLength = t - seen(hash)
            val remainingLoops = (lim - t - 1)  / loopLength
            t += remainingLoops * loopLength
        } else {
            seen(hash) = t
        }
        
        mtx = step(mtx)
        t += 1
    }

    val frequencies = mtx.flatten.groupMapReduce(identity)(_ => 1)(_ + _)

    return frequencies('|') * frequencies('#')
}

def evaluatorOne(input: List[List[Char]]): Int = iterate(input, 10)
def evaluatorTwo(input: List[List[Char]]): Int = iterate(input, 1000000000)

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