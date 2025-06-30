package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

val raceTotalTime = 2503

case class Reindeer(speed: Int, durationTime: Int, restTime: Int) {
    val restartTime = durationTime + restTime
}

def parseInput(input: List[String]) = input.map(line => {
    val Seq(a, b, c) = raw"(\d+)".r.findAllIn(line).map(_.toInt).toSeq
    Reindeer(a, b, c)
})

def totalDistance(reindeer: Reindeer, totalTime: Int): Int = {
    var totalRuns = totalTime / reindeer.restartTime
    val remainingTime = totalTime % reindeer.restartTime
    if remainingTime >= reindeer.durationTime then totalRuns += 1
    return reindeer.speed * reindeer.durationTime * totalRuns
}

def distanceTravelledEverySecond(reindeer: Reindeer, totalTime: Int): List[Int] = {
    val result = List.tabulate(totalTime) { i => if (i % reindeer.restartTime < reindeer.durationTime) 1 else 0 }
    return result.scanLeft(0)(_ + _).map(_ * reindeer.speed).tail
}

def evaluatorOne(reindeers: List[Reindeer]): Int = reindeers.map(totalDistance(_, raceTotalTime)).max

def evaluatorTwo(reindeers: List[Reindeer]): Int = {
    val raceTimeStamps = reindeers.map(distanceTravelledEverySecond(_, raceTotalTime)).transpose

    val playersById = raceTimeStamps.flatMap(raceTimeStamp => {
        val maxDistance = raceTimeStamp.max
        raceTimeStamp.zipWithIndex.collect { case (distance, id) if distance == maxDistance => id }
    })

    return playersById.groupMapReduce(identity)(_ => 1)(_ + _).values.max
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