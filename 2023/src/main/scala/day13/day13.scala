package day13

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer

case class Point(y: Int, x: Int) {
    def swap = Point(x, y)
}

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def parseGrid(input: List[String]): Set[Point] = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
        if ch == '#'
    } yield Point(y, x)).toSet
}

def parseInput(input: List[String]) = groupLines(input).map(parseGrid)

def findVerticalReflection(points: Set[Point], smudges: Int = 0): Int = {
    val maxX = points.map(_.x).max
    
    val index = (0 until maxX).find(x => {
        val reflection = x + 1
        
        val differences = points.count(point => {
            val reflectedX = 2 * reflection - point.x - 1
            if (reflectedX < 0 || reflectedX > maxX) then false else {
                val reflectedPoint = Point(reflectedX, point.y)
                points.contains(point) != points.contains(reflectedPoint)
            }
        })
        
        differences == smudges
    })
    
    return index.map(_ + 1).getOrElse(0)
}

def findHorizontalReflection(points: Set[Point], smudges: Int): Int = {
    val swappedPoints = points.map(_.swap)
    return findVerticalReflection(swappedPoints, smudges)
}

def summarize(patterns: List[Set[Point]], smudges: Int): Int = {
    return patterns.map(points => {
        findHorizontalReflection(points, smudges) * 100 +
        findVerticalReflection(points, smudges) 
    }).sum
}

def evaluatorOne(input: List[Set[Point]]) = summarize(input, 0)
def evaluatorTwo(input: List[Set[Point]]) = summarize(input, 1)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day13.txt") match {
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