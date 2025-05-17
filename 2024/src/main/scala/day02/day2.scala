package day02

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: List[String]) = input.map(_.split(" ").map(_.toInt).toList)

def isValid(samples: List[Int]): Boolean = {
    val diffArray = (samples.init zip samples.tail).map { case (a, b) => b - a }

    diffArray.forall(diff => diff >= 1 && diff <= 3) ||
    diffArray.forall(diff => diff >= -3 && diff <= -1)
}

def attenuate(samples: List[Int]) = {
    val skippedList = samples.indices.map(i => {
        samples.take(i) ++ samples.drop(i + 1)    
    })

    samples +: skippedList
}

def evaluatorOne(input: List[List[Int]]): Int = input.count(isValid)
def evaluatorTwo(input: List[List[Int]]): Int = input.map(attenuate).count(_.exists(isValid))

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day02.txt") match {
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