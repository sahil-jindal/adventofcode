package day16

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

val criteria = Map(
    "children" -> 3,
    "cats" -> 7,
    "samoyeds" -> 2,
    "pomeranians" -> 3,
    "akitas" -> 0,
    "vizslas" -> 0,
    "goldfish" -> 5,
    "trees" -> 3,
    "cars" -> 2,
    "perfumes" -> 1
)

def parseInput(lines: List[String]): List[(Int, Map[String, Int])] = {
    return lines.map(line => {
        val parts = line.split(": ", 2)
        val sueNumber = parts(0).split(" ")(1).toInt

        val properties = parts(1).split(", ").map { prop =>
            val Array(key, value) = prop.split(": ")
            key -> value.toInt
        }.toMap

        sueNumber -> properties
    })
}

def matchFunctionOne(key: String, value: Int): Boolean = criteria(key) == value

def matchFunctionTwo(key: String, value: Int): Boolean = key match {
    case "cats" | "trees" => criteria(key) < value
    case "pomeranians" | "goldfish" => criteria(key) > value
    case _ => criteria(key) == value
}

def findAuntySue(aunts: List[(Int, Map[String, Int])], matchFunction: (String, Int) => Boolean): Int = {
    def checkProperties(properties: Map[String, Int]): Boolean = properties.forall { case (key, value) => matchFunction(key, value) }
    return aunts.collectFirst { case (sueNumber, properties) if checkProperties(properties) => sueNumber }.get
}

def evaluatorOne(aunts: List[(Int, Map[String, Int])]): Int = findAuntySue(aunts, matchFunctionOne)
def evaluatorTwo(aunts: List[(Int, Map[String, Int])]): Int = findAuntySue(aunts, matchFunctionTwo)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day16.txt") match {
        case Success(lines) => {
            val aunts = parseInput(lines)
            println(s"Part One: ${evaluatorOne(aunts)}")
            println(s"Part Two: ${evaluatorTwo(aunts)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}