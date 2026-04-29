package day02

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

extension [A](items: List[A]) {
    def upperTriangle(): List[(A, A)] = {
        val partOne = items.tail.tails.toVector
        return (items zip partOne).init.flatMap {
            case (e1, ahead) => ahead.map(e1 -> _)
        }
    }
}

def getFrequency(input: String): List[Int] = {
    return input.groupMapReduce(identity)(_ => 1)(_ + _).values.toList
}

def diff(line1: String, line2: String): Int = {
    return (line1 zip line2).count(_ != _)
}

def common(line1: String, line2: String): String = {
    return (line1 zip line2).collect { case (a, b) if a == b => a }.mkString
}

def evaluatorOne(input: List[String]): Int = {
    val freqlist = input.map(getFrequency)
    val doubles = freqlist.count(_.exists(_ == 2))
    val triples = freqlist.count(_.exists(_ == 3))
    return doubles * triples
}

def evaluatorTwo(input: List[String]): String = {
    return input.upperTriangle().collectFirst {
        case (a, b) if diff(a, b) == 1 => common(a, b)
    }.get
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day02.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}