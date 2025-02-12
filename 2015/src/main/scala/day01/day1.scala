package day01

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.boundary, boundary.break

def floorMovement(ch: Char): Int = ch match {
    case '(' => 1
    case ')' => -1
    case _ => 0
}

def evaluatorOne(line: String): Int = {
    return line.foldLeft(0) { case (acc, it) => acc + floorMovement(it) }
}

def evaluatorTwo(line: String): Int = {
    var sum = 0

    boundary {
        for (ch, i) <- line.zipWithIndex do {
            sum += floorMovement(ch)
            if sum == -1 then break(i + 1) 
        }

        return -1
    }
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day01.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines.head)}")
            println(s"Part Two: ${evaluatorTwo(lines.head)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}