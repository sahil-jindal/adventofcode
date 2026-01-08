package day19

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

// computes the number of ways the pattern can be build up from the towels. 
// works recursively by matching the prefix of the pattern with each towel.
// a full match is found when the pattern becomes empty. the cache is applied 
// to _drammatically_ speed up execution

def matchCount(towels: List[String], pattern: String, cache: Map[String, Long]): Long = {
    cache.getOrElseUpdate(pattern, {
        if (pattern.isBlank()) 1L else towels.withFilter(pattern.startsWith)
            .map(it => matchCount(towels, pattern.substring(it.length), cache)).sum
    })
}

def matchCounts(input: List[String]): List[Long] = {
    val towels = input.head.split(", ").toList
    return input.drop(2).map(pattern => matchCount(towels, pattern, Map.empty))
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day19.txt") match {
        case Success(lines) => {
            val input = matchCounts(lines).filter(_ != 0)
            println(s"Part One: ${input.size}")
            println(s"Part Two: ${input.sum}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}