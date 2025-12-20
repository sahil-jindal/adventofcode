package day02

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.matching.Regex

case class Game(id: Int, red: Int, green: Int, blue: Int)

def parseInts(st: String, rx: Regex): List[Int] = {
    return rx.findAllMatchIn(st).map(_.group(1).toInt).toList
}

def parseInput(input: List[String]) = input.map(line => {
    new Game(
        parseInts(line, raw"Game (\d+)".r).head,
        parseInts(line, raw"(\d+) red".r).max,
        parseInts(line, raw"(\d+) green".r).max,
        parseInts(line, raw"(\d+) blue".r).max
    )
})

def evaluatorOne(games: List[Game]): Int = {
    return games.withFilter(g => g.red <= 12 && g.green <= 13 && g.blue <= 14).map(_.id).sum
}

def evaluatorTwo(games: List[Game]): Int = games.map(g => g.red * g.green * g.blue).sum

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