package daythirteen

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

def parseInput(lines: List[String]): Map[(String, String), Int] = {
    val happiness = Map[(String, String), Int]()
    
    for (line <- lines) {
        val parts = line.split(" ")
        val person1 = parts(0)
        val person2 = parts.last.dropRight(1) // Remove the period
        val value = parts(3).toInt * (if (parts(2) == "gain") 1 else -1)
        happiness((person1, person2)) = value
    }

    happiness
}

def calculateHappiness(arrangement: Seq[String], happiness: Map[(String, String), Int]): Int = {
    val n = arrangement.length
    arrangement.indices.map { i =>
        val person1 = arrangement(i)
        val person2 = arrangement((i + 1) % n) // Circular table
        happiness.getOrElse((person1, person2), 0) + happiness.getOrElse((person2, person1), 0)
    }.sum
}

def addYourself(happiness: Map[(String, String), Int]): Map[(String, String), Int] = {
    val guests = happiness.keys.flatMap { case (p1, p2) => Seq(p1, p2) }.toSet
    val updatedHappiness = Map(happiness.toSeq*)
    
    for (guest <- guests) {
        updatedHappiness(("You", guest)) = 0
        updatedHappiness((guest, "You")) = 0
    }

    updatedHappiness
  }

def findMaximumHappiness(happiness: Map[(String, String), Int]): Int = {
    val guests = happiness.keys.flatMap { case (p1, p2) => Seq(p1, p2) }.toSet
    
    guests.toSeq.permutations.map { arrangement =>
        calculateHappiness(arrangement, happiness)
    }.max
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("daythirteen.txt") match
        case Success(lines) => {
            val happiness = parseInput(lines)
            var maxHappiness = findMaximumHappiness(happiness)
            println(s"Maximum Total Happiness: $maxHappiness")

            val happinessWithYou = addYourself(happiness)
            maxHappiness = findMaximumHappiness(happinessWithYou)
            println(s"Maximum Total Happiness (including yourself): $maxHappiness")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }