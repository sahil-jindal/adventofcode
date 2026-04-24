package day02

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.matching.Regex

type Game = (id: Int, red: Int, green: Int, blue: Int)

def parseInts(st: String, rx: Regex): List[Int] = {
    return rx.findAllMatchIn(st).map(_.group(1).toInt).toList
}

def parseInput(input: List[String]) = input.map(line => {
    val id = parseInts(line, raw"Game (\d+)".r).head
    val r = parseInts(line, raw"(\d+) red".r).max
    val g = parseInts(line, raw"(\d+) green".r).max
    val b = parseInts(line, raw"(\d+) blue".r).max

    (id, r, g, b)
})

def evaluatorOne(games: List[Game]): Int = {
    return (for {
        (id, r, g, b) <- games
        if r <= 12 && g <= 13 && b <= 14
    } yield id).sum
}

def evaluatorTwo(games: List[Game]): Int = {
    return games.map { case (_, r, g, b) => r * g * b }.sum
}

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