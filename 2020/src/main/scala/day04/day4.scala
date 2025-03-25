package day04

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

val rxs = Map(
    "byr" -> "19[2-9][0-9]|200[0-2]",
    "iyr" -> "201[0-9]|2020",
    "eyr" -> "202[0-9]|2030",
    "hgt" -> "1[5-8][0-9]cm|19[0-3]cm|59in|6[0-9]in|7[0-6]in",
    "hcl" -> "#[0-9a-f]{6}",
    "ecl" -> "amb|blu|brn|gry|grn|hzl|oth",
    "pid" -> "[0-9]{9}"
)

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]  // Start a new sublist on empty string
        case (acc, elem) => acc.init :+ (acc.last :+ elem) // Append element to the last sublist
    }.filter(_.nonEmpty)
}

def parseInput(input: List[String]): List[Map[String, String]] = {
    return groupLines(input).map(passport => {
        passport.flatMap(_.split(" ")).map(_.split(":")).collect {
            case Array(key, value) => key -> value
        }.toMap
    })
}

def evaluatorOne(input: List[Map[String, String]]): Int = {
    return input.count(cred => rxs.keys.forall(cred.contains))
}

def evaluatorTwo(input: List[Map[String, String]]): Int = {
    return input.count(cred => rxs.forall { case (key, pattern) => 
        cred.get(key).exists(value => pattern.r.matches(value))
    })
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day04.txt") match {
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