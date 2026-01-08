package day16

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Aunt(sueNumber: Int, properties: Map[String, Int])

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

def parseInput(input: List[String]) = input.map(line => {
    val Array(partOne, partTwo) = line.split(": ", 2)
    val sueNumber = partOne.stripPrefix("Sue ").toInt

    val properties = partTwo.split(", ").map(prop => {
        val Array(key, value) = prop.split(": ")
        key -> value.toInt
    }).toMap

    Aunt(sueNumber, properties)
})

def matchFunctionOne(key: String, value: Int): Boolean = criteria(key) == value

def matchFunctionTwo(key: String, value: Int): Boolean = key match {
    case "cats" | "trees" => criteria(key) < value
    case "pomeranians" | "goldfish" => criteria(key) > value
    case _ => criteria(key) == value
}

def findAuntySue(aunts: List[Aunt], matchFunction: ((String, Int)) => Boolean): Int = {
    return aunts.find(_.properties.forall(matchFunction)).get.sueNumber
}

def evaluatorOne(aunts: List[Aunt]): Int = findAuntySue(aunts, matchFunctionOne)
def evaluatorTwo(aunts: List[Aunt]): Int = findAuntySue(aunts, matchFunctionTwo)

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