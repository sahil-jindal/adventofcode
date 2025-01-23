package daysixteen

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

def parseInput(line: String) = {
    val parts = line.split(": ", 2)
    val sueNumber = parts(0).split(" ")(1).toInt
    
    val properties = parts(1).split(", ").map { prop =>
        val Array(key, value) = prop.split(": ")
        key -> value.toInt
    }.toMap
    
    sueNumber -> properties
}

def matchFunctionOne(key: String, value: Int): Boolean = criteria(key) == value

def matchFunctionTwo(key: String, value: Int): Boolean = key match {
    case "cats" | "trees" => criteria(key) < value
    case "pomeranians" | "goldfish" => criteria(key) > value
    case _ => criteria(key) == value
}

def findAuntySue(aunts: Array[(Int, Map[String, Int])], matchFunction: (String, Int) => Boolean) = {
    val matchingAunt = aunts.find { case (_, properties) =>
        properties.forall { case (key, value) =>
            matchFunction(key, value)
        }
    }

    matchingAunt match {
        case Some((sueNumber, _)) => println(s"Aunt Sue $sueNumber got you the gift!")
        case None => println("No matching Aunt Sue found!")
    }
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

@main
def hello(): Unit =
    readLinesFromFile("daysixteen.txt") match
        case Success(lines) => {
            val aunts = lines.map(parseInput).toArray
            findAuntySue(aunts, matchFunctionTwo)
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }