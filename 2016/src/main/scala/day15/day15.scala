package day15

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Disc(id: Int, pos: Int, mod: Int)

val pattern = raw"Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+).".r

def parseInput(input: List[String]) = input.map(line => {
    val Seq(id, mod, pos) = pattern.findFirstMatchIn(line).get.subgroups.map(_.toInt)
    Disc(id, pos, mod)
})

def iterate(discs: List[Disc]): Int = Iterator.from(0).find(t => 
    discs.forall { case Disc(id, pos, mod) => (pos + t + id) % mod == 0 }
).get

def evaluatorOne(discs: List[Disc]): Int = iterate(discs)
def evaluatorTwo(discs: List[Disc]): Int = iterate(discs :+ Disc(discs.size + 1, 0, 11)) 

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