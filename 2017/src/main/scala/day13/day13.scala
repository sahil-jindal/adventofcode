package day13

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Layer(depth: Int, range: Int)

def parseInput(input: List[String]) = input.collect {
    case s"$depth: $range" => Layer(depth.toInt, range.toInt)
}

def severities(layers: List[Layer], t: Int): List[Int] = {
    return layers.withFilter(it => (t + it.depth) % (2 * it.range - 2) == 0).map(it => it.depth * it.range)
}

def evaluatorOne(layers: List[Layer]): Int = severities(layers, 0).sum
def evaluatorTwo(layers: List[Layer]): Int = Iterator.from(0).find(severities(layers, _).isEmpty).get

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day13.txt") match {
        case Success(lines) => {
            val layers = parseInput(lines)
            println(s"Part One: ${evaluatorOne(layers)}")
            println(s"Part Two: ${evaluatorTwo(layers)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}