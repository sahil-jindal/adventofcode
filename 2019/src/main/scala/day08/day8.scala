package day08

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: String): List[Seq[Int]] = input.map(_.asDigit).grouped(6 * 25).toList

def evaluatorOne(layers: List[Seq[Int]]): Int = {
    val layer = layers.minBy(_.count(_ == 0))
    val ones = layer.count(_ == 1)
    val twos = layer.count(_ == 2)
    return ones * twos
}

def evaluatorTwo(layers: List[Seq[Int]]): String = {
    val img = Array.fill(6 * 25)(' ')

    for (layer <- layers.reverse; i <- img.indices) {
        img(i) = layer(i) match {
            case 0 => ' '
            case 1 => '#'
            case _ => img(i)
        }
    }

    return img.grouped(25).map(_.mkString).mkString("\n")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day08.txt") match {
        case Success(lines) => {
            val layers = parseInput(lines.head)
            println(s"Part One: ${evaluatorOne(layers)}")
            println(s"Part Two:\n${evaluatorTwo(layers)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}