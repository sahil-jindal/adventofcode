package day06

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

type Freq = Map[Char, Int]

def parseInput(input: List[String]) = input.transpose.map(line => {
    line.groupMapReduce(identity)(_ => 1)(_ + _)
})

def evaluatorOne(input: List[Freq]) = input.map(_.maxBy(_._2)._1).mkString
def evaluatorTwo(input: List[Freq]) = input.map(_.minBy(_._2)._1).mkString

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day06.txt") match {
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