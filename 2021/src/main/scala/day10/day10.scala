package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Stack
import scala.util.boundary, boundary.break

def getScore(line: String, getSyntaxErrorScore: Boolean): Long = {
    val stack = Stack(line.head)

    boundary {
        for (ch <- line.tail) {
            (stack.top, ch) match {
                case ('(', ')') | ('[', ']') | ('{', '}') | ('<', '>') => stack.pop()
                case (_, ')') => break(if (getSyntaxErrorScore) 3L     else 0L)
                case (_, ']') => break(if (getSyntaxErrorScore) 57L    else 0L)
                case (_, '}') => break(if (getSyntaxErrorScore) 1197L  else 0L)
                case (_, '>') => break(if (getSyntaxErrorScore) 25137L else 0L)
                case _        => stack.push(ch)
            }
        }

        if (getSyntaxErrorScore) return 0L
    
        return stack.map(ch => 1 + "([{<".indexOf(ch)).foldLeft(0L) { case (acc, item) => acc * 5 + item }
    }
}

def getScores(input: List[String], getSyntaxErrorScore: Boolean): List[Long] = {
    return input.map(getScore(_, getSyntaxErrorScore)).filter(_ > 0)
}

def evaluatorOne(input: List[String]): Long = getScores(input, true).sum

def evaluatorTwo(input: List[String]): Long = {
    val scores = getScores(input, false).sorted
    return scores(scores.size / 2)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day10.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}