package day03

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Set

case class Point(y: Int, x: Int)

def run(input: String, actors: Int): Int = {
    val seen = Set(Point(0, 0))
    val pos = Array.fill(actors)(Point(0, 0))

    for ((ch, i) <- input.zipWithIndex) { 
        val Point(y, x) = pos(i % actors)
        
        pos(i % actors) = ch match {
            case 'v' => Point(y + 1, x)
            case '<' => Point(y, x - 1)
            case '>' => Point(y, x + 1)
            case '^' => Point(y - 1, x)
            case _   => Point(y, x) // Should not happen
        }

        seen += pos(i % actors)
    }
    
    return seen.size
}

def evaluatorOne(input: String): Int = run(input, 1)
def evaluatorTwo(input: String): Int = run(input, 2)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day03.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines.head)}")
            println(s"Part Two: ${evaluatorTwo(lines.head)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}