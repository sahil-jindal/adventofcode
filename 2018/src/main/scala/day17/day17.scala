package day17

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.boundary, boundary.break

def isStill(mtx: Array[Array[Char]], x: Int, y: Int): Boolean = {
    val width = mtx(0).length
    
    boundary {
        for (dx <- Seq(-1, 1)) {
            var xT = x
            while (xT >= 0 && xT < width && mtx(y)(xT) != '#') {
                if (mtx(y)(xT) == '.' || mtx(y + 1)(xT) == '|') break(false)
                xT += dx
            }
        }

        true
    }
}

def fillRecursive(mtx: Array[Array[Char]], x: Int, y: Int): Unit = {
    val height = mtx.length
    val width = mtx(0).length
    
    if (mtx(y)(x) != '.') return
    
    mtx(y)(x) = '|'
    
    if (y == height - 1) return

    fillRecursive(mtx, x, y + 1)

    if (mtx(y + 1)(x) == '#' || mtx(y + 1)(x) == '~') {
        if (x > 0) fillRecursive(mtx, x - 1, y)
        if (x < width - 1) fillRecursive(mtx, x + 1, y)
    }

    if (isStill(mtx, x, y)) {
        for (dx <- Seq(-1, 1)) {
            var xT = x
            while (xT >= 0 && xT < width && mtx(y)(xT) == '|') {
                mtx(y)(xT) = '~'
                xT += dx
            }
        }
    }
}

def solver(input: List[String]): Map[Char, Int] = {
    val mtx = Array.fill(2000, 2000)('.')

    for (line <- input) {
        val Seq(a, b, c) = raw"(\d+)".r.findAllIn(line).map(_.toInt).toSeq
        
        for (i <- b to c) {
            if (line.startsWith("x")) mtx(i)(a) = '#' 
            else mtx(a)(i) = '#'
        }
    }
    
    fillRecursive(mtx, 500, 0)
    
    val minY = mtx.indexWhere(_.exists(_ == '#'))
    val maxY = mtx.lastIndexWhere(_.exists(_ == '#'))
    
    return mtx.slice(minY, maxY + 1).flatten.groupMapReduce(identity)(_ => 1)(_ + _)
}

def evaluatorOne(frequencies: Map[Char, Int]): Int = frequencies('~') + frequencies('|')
def evaluatorTwo(frequencies: Map[Char, Int]): Int = frequencies('~')

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day17.txt") match {
        case Success(lines) => {
            val input = solver(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}