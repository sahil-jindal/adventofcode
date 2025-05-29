package day02

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Box(l: Int, b: Int, h: Int)

def parseInput(input: List[String]) = input.map(line => {
    val boxes = line.split("x").map(_.toInt).sorted
    Box(boxes(0), boxes(1), boxes(2))
})

def evaluatorOne(boxes: List[Box]): Int = boxes.map { case Box(a, b, c) => 3*a*b + 2*a*c + 2*b*c }.sum
def evaluatorTwo(boxes: List[Box]): Int = boxes.map { case Box(a, b, c) => 2*(a + b) + a*b*c }.sum

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day02.txt") match {
        case Success(lines) => {
            val boxes = parseInput(lines)
            println(s"Part One: ${evaluatorOne(boxes)}")
            println(s"Part One: ${evaluatorTwo(boxes)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}