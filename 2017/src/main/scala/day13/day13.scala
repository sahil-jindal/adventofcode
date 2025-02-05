package day13

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer

case class Layer(depth: Int, range: Int)
type Layers = List[Layer]

def parseInput(input: List[String]): Layers = {
    input.map { line =>
        val parts = line.split(": ").map(_.toInt)
        Layer(parts(0), parts(1))
    }.toList
}

def severities(layers: Layers, t: Int): List[Int] = {
    val result = ListBuffer[Int]()
    var time = t
    var packetPos = 0

    for layer <- layers do {
        time += layer.depth - packetPos
        packetPos = layer.depth
        val scannerPos = time % (2 * layer.range - 2)
        if scannerPos == 0 then result += layer.depth * layer.range
    }
    
    return result.toList
}

def evaluatorOne(layers: Layers): Int = severities(layers, 0).sum
def evaluatorTwo(layers: Layers): Int = LazyList.from(0).find(n => severities(layers, n).isEmpty).get

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("day13.txt") match
        case Success(lines) => {
            val layers = parseInput(lines)
            println(s"Part One: ${evaluatorOne(layers)}")
            println(s"Part Two: ${evaluatorTwo(layers)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }