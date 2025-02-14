package day09

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def process(input: String): (Int, Int) = {
    var inGarbage = false
    var ignoreNext = false
    var depth = 0
    var totalScore = 0
    var garbageCount = 0

    for (c <- input) {
        if (ignoreNext) {
            ignoreNext = false
        } else {
            if (inGarbage) {
                c match {
                    case '!' => ignoreNext = true
                    case '>' => inGarbage = false
                    case _ => garbageCount += 1
                }
            } else {
                c match {
                    case '!' => ignoreNext = true
                    case '<' => inGarbage = true
                    case '{' => depth += 1
                    case '}' => totalScore += depth; depth -= 1
                    case _ => // Ignore other characters outside garbage
                }
            }
        }
    }

    return (totalScore, garbageCount)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day09.txt") match {
        case Success(lines) => {
            val (score, garbage) = process(lines.head)
            println(s"Part One: $score")
            println(s"Part Two: $garbage")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}