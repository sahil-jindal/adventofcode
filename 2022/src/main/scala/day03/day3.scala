package day03

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def getCommonItemPriority(texts: List[String]): Int = {
    val ch = texts.map(_.toSet).reduce(_ & _).head
    
    if (('A' to 'Z').contains(ch)) return ch - 'A' + 27 
    if (('a' to 'z').contains(ch)) return ch - 'a' + 1

    return ch
}

def solver(input: List[List[String]]): Int = input.map(getCommonItemPriority).sum

def transformationOne(input: List[String]) = input.map(it => it.grouped(it.length / 2).toList)
def transformationTwo(input: List[String]) = input.grouped(3).toList

def evaluatorOne(input: List[String]): Int = solver(transformationOne(input))
def evaluatorTwo(input: List[String]): Int = solver(transformationTwo(input))

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day03.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}