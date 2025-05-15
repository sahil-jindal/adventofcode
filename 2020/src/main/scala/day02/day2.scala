package day02

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class PasswordEntry(a: Int, b: Int, ch: Char, password: String)

val regex = raw"(\d+)-(\d+) (\w): (\w+)".r

def parseInput(input: List[String]) = input.map(line => {
    val List(a, b, ch, password) = regex.findFirstMatchIn(line).get.subgroups
    PasswordEntry(a.toInt, b.toInt, ch.head, password)
})

def evaluatorOne(input: List[PasswordEntry]): Int = input.count(pe => {
    val count = pe.password.count(_ == pe.ch)
    pe.a <= count && count <= pe.b
})

def evaluatorTwo(input: List[PasswordEntry]): Int = input.count(pe => {
    (pe.password(pe.a - 1) == pe.ch) ^ (pe.password(pe.b - 1) == pe.ch)
})

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