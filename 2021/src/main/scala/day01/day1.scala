package day01

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: List[String]) = input.map(_.toInt)

def depthIncrease(ns: List[Int]): Int = (ns.init zip ns.tail).count { case (a, b) => a < b }

def threeMeasurements(ns: List[Int]) = ns.sliding(3).map(_.sum).toList

def evaluatorOne(ns: List[Int]): Int = depthIncrease(ns)
def evaluatorTwo(ns: List[Int]): Int = depthIncrease(threeMeasurements(ns))

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day01.txt") match {
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