package day03

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Set

case class Point(val y: Int, val x: Int)

def run(input: String, actors: Int): Int = {
    val seen = Set(Point(0, 0))
    val pos = Array.fill(actors)(Point(0, 0))

    input.zipWithIndex.foreach { case (ch, i) =>
        val Point(y, x) = pos(i % actors)
        
        pos(i % actors) = ch match {
            case 'v' => Point(y + 1, x)
            case '<' => Point(y, x - 1)
            case '>' => Point(y, x + 1)
            case '^' => Point(y - 1, x)
            case _   => Point(y, x) // Should not happen
        }

        seen.add(pos(i % actors))
    }
    
    seen.size
}

def evaluatorOne(line: String): Int = run(line, 1)
def evaluatorTwo(line: String): Int = run(line, 2)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("day03.txt") match
        case Success(lines) => {
            val line = lines(0)
            println(s"Part One: ${evaluatorOne(line)}")
            println(s"Part Two: ${evaluatorTwo(line)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }