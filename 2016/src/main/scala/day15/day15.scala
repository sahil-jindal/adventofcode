package day15

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

val pattern = """Disc #\d+ has (\d+) positions; at time=0, it is at position (\d+).""".r

case class Disc(val pos: Int, val mod: Int)

def parseInput(input: List[String]): List[Disc] = input.collect {
    case pattern(mod, pos) => Disc(pos.toInt, mod.toInt)
}

def iterate(discs: List[Disc]): Int = Iterator.from(0).find(t => 
    discs.zipWithIndex.forall { case (disc, i) => (disc.pos + t + i + 1) % disc.mod == 0 }
).get

def evaluatorOne(discs: List[Disc]): Int = iterate(discs)
def evaluatorTwo(discs: List[Disc]): Int = iterate(discs :+ Disc(0, 11)) 

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