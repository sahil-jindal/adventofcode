package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

val raceTotalTime = 2503

case class Reindeer(speed: Int, fly: Int, rest: Int) {
    def distance(time: Int): Int = {
        val cycleTime = fly + rest
        var complete = time / cycleTime
        val partial = (time % cycleTime).min(fly)
        return speed * (fly * complete + partial)
    }
}

def parseInput(input: List[String]) = input.map(line => {
    val Seq(a, b, c) = raw"(\d+)".r.findAllIn(line).map(_.toInt).toSeq
    Reindeer(a, b, c)
})

def evaluatorOne(reindeers: List[Reindeer]): Int = {
    return reindeers.map(_.distance(raceTotalTime)).max
}

def evaluatorTwo(reindeers: List[Reindeer]): Int = {
    val pointsCollection = Array.ofDim[Int](reindeers.length)

    for (t <- 1 to raceTotalTime) {
        val distances = reindeers.map(_.distance(t))
        val lead = distances.max
        
        distances.iterator.zipWithIndex
            .collect { case (dist, id) if dist == lead => id }
            .foreach { i => pointsCollection(i) += 1 }
    }

    return pointsCollection.max
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day14.txt") match {
        case Success(lines) => {
            val reindeers = parseInput(lines)
            println(s"Part One: ${evaluatorOne(reindeers)}")
            println(s"Part Two: ${evaluatorTwo(reindeers)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}