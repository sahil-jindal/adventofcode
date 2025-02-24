package day17

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.StringBuilder
import scala.util.boundary, boundary.break;

def isStill(mtx: Array[Array[Char]], x: Int, y: Int): Boolean = {
    val width = mtx.length
    
    boundary {
        for (dx <- Seq(-1, 1)) {
            var xT = x
            while (xT >= 0 && xT < width && mtx(xT)(y) != '#') {
                if (mtx(xT)(y) == '.' || mtx(xT)(y + 1) == '|') break(false)
                xT += dx
            }
        }

        true
    }
}

def fillRecursive(mtx: Array[Array[Char]], x: Int, y: Int): Unit = {
    val width = mtx.length
    val height = mtx(0).length
    
    if (mtx(x)(y) != '.') return
    
    mtx(x)(y) = '|'
    
    if (y == height - 1) return

    fillRecursive(mtx, x, y + 1)

    if (mtx(x)(y + 1) == '#' || mtx(x)(y + 1) == '~') {
        if (x > 0) fillRecursive(mtx, x - 1, y)
        if (x < width - 1) fillRecursive(mtx, x + 1, y)
    }

    if (isStill(mtx, x, y)) {
        for (dx <- Seq(-1, 1)) {
            var xT = x
            while (xT >= 0 && xT < width && mtx(xT)(y) == '|') {
                mtx(xT)(y) = '~'
                xT += dx
            }
        }
    }
}

def fill(lines: List[String]): String = {
    val (width, height) = (2000, 2000)
    val mtx = Array.fill(width, height)('.')
    val numberPattern = """\d+""".r

    lines.foreach(line => {
        val nums = numberPattern.findAllIn(line).map(_.toInt).toArray
        
        for (i <- nums(1) to nums(2)) {
            if (line.startsWith("x")) mtx(nums(0))(i) = '#' 
            else mtx(i)(nums(0)) = '#'
        }
    })
    
    fillRecursive(mtx, 500, 0)

    var (minY, maxY) = (Int.MaxValue, Int.MinValue)
    
    for (y <- 0 until height; x <- 0 until width) {
        if (mtx(x)(y) == '#') {
            minY = math.min(minY, y)
            maxY = math.max(maxY, y)
        }
    }

    val sb = new StringBuilder
    
    for (y <- minY to maxY) {
        for (x <- 0 until width) {
            sb.append(mtx(x)(y))
        }
        
        sb.append("\n")
    }

    sb.toString
}

def evaluatorOne(input: List[String]): Int = "[~|]".r.findAllMatchIn(fill(input)).length
def evaluatorTwo(input: List[String]): Int = "[~]".r.findAllMatchIn(fill(input)).length

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day17.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}