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

def fill(input: List[String]): Array[Char] = {
    val (width, height) = (2000, 2000)
    val mtx = Array.fill(height, width)('.')

    for (line <- input) {
        val nums = raw"(\d+)".r.findAllIn(line).map(_.toInt).toArray
        
        for (i <- nums(1) to nums(2)) {
            if (line.startsWith("x")) mtx(i)(nums(0)) = '#' 
            else mtx(nums(0))(i) = '#'
        }
    }
    
    fillRecursive(mtx, 500, 0)

    var (minY, maxY) = (Int.MaxValue, Int.MinValue)
    
    for ((row, y) <- mtx.zipWithIndex) {
        if (row.exists(_ == '#')) {
            minY = math.min(minY, y)
            maxY = math.max(maxY, y)
        }
    }
    
    return mtx.slice(minY, maxY + 1).flatten
}

def evaluatorOne(input: List[Char]): Int = input.count(it => it == '~' || it == '|')
def evaluatorTwo(input: List[Char]): Int = input.count(it => it == '~')

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day17.txt") match {
        case Success(lines) => {
            val lastScene = fill(lines).toList
            println(s"Part One: ${evaluatorOne(lastScene)}")
            println(s"Part Two: ${evaluatorTwo(lastScene)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}