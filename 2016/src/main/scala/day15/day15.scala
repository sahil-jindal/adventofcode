package day15

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

type Disc = (size: Int, position: Int)

val pattern = raw"Disc #\d has (\d+) positions; at time=0, it is at position (\d+).".r

def parseInput(input: List[String]) = input.collect {
    case pattern(mod, pos) => (mod.toInt, pos.toInt)
}

def iterate(discs: List[Disc]): Int = {
    var (time, step) = (0, 1)

    for (((size, position), offset) <- discs.zipWithIndex) {
        while ((time + offset + 1 + position) % size != 0) { time += step }
        step *= size
    }

    return time
}

def evaluatorOne(discs: List[Disc]): Int = iterate(discs)
def evaluatorTwo(discs: List[Disc]): Int = iterate(discs :+ (11, 0)) 

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day15.txt") match {
        case Success(lines) => {
            val discs = parseInput(lines)
            println(s"Part One: ${evaluatorOne(discs)}") 
            println(s"Part Two: ${evaluatorTwo(discs)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}