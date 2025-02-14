package day13

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Layer(depth: Int, range: Int)

def parseInput(input: List[String]): List[Layer] = input.map(line => {
    val Array(depth, range) = line.split(": ").map(_.toInt)
    Layer(depth, range)
})

def severities(layers: List[Layer], t: Int): List[Int] = {
    return layers.filter(it => (t + it.depth) % (2 * it.range - 2) == 0).map(it => it.depth * it.range)
}

def evaluatorOne(layers: List[Layer]): Int = severities(layers, 0).sum
def evaluatorTwo(layers: List[Layer]): Int = Iterator.from(0).find(n => severities(layers, n).isEmpty).get

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

@main
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