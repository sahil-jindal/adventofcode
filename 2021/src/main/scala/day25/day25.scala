package day25

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def evaluatorOne(input: List[String]): Int = {
    var map = input
    val height = map.length
    val width = map(0).length

    def right(x: Int): Int = (x + 1) % width
    def down(y: Int): Int = (y + 1) % height
    def left(x: Int): Int = (x - 1 + width) % width
    def up(y: Int): Int = (y - 1 + height) % height

    def movesRight(y: Int, x: Int): Boolean = map(y)(x) == '>' && map(y)(right(x)) == '.'
    def movesDown(y: Int, x: Int): Boolean = map(y)(x) == 'v' && map(down(y))(x) == '.'

    return Iterator.from(1).find(steps => {
        var anyMoves = false
        var newMap = Array.ofDim[String](height)

        for (y <- 0 until height) {
            val sb = new StringBuilder

            for (x <- 0 until width) {
                if (movesRight(y, x)) anyMoves = true
                
                val ch =
                    if (movesRight(y, x)) '.'
                    else if (movesRight(y, left(x))) '>'
                    else map(y)(x)
            
                sb.append(ch)
            }

            newMap(y) = sb.toString()
        }

        map = newMap.toList
        newMap = Array.ofDim[String](height)

        for (y <- 0 until height) {
            val sb = new StringBuilder
            
            for (x <- 0 until width) {
                if (movesDown(y, x)) anyMoves = true
                
                val ch =
                    if (movesDown(y, x)) '.'
                    else if (movesDown(up(y), x)) 'v'
                    else map(y)(x)

                sb.append(ch)
            }

            newMap(y) = sb.toString()
        }

        map = newMap.toList

        !anyMoves
    
    }).get
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day25.txt") match {
        case Success(lines) => println(s"Part One: ${evaluatorOne(lines)}")
        case Failure(exception) => println(s"Error reading file: ${exception.getMessage}")
    }
}