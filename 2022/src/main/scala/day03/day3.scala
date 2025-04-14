package day03

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def getCommonItemPriority(texts: List[String]): Int = {
    return texts.head.collectFirst { case ch if texts.forall(_.contains(ch)) => if ch < 'a' then ch - 'A' + 27 else ch - 'a' + 1 }.get
}

def evaluatorOne(input: List[String]) = input.map(it => getCommonItemPriority(it.grouped(it.length / 2).toList)).sum
def evaluatorTwo(input: List[String]) = input.grouped(3).map(getCommonItemPriority).sum

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