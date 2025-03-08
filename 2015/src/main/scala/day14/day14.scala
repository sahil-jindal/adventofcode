package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

val raceTotalTime = 2503

class Reindeer(val speed: Int, val durationTime: Int, val restTime: Int)

def parseInput(lines: List[String]): List[Reindeer] = lines.map(line => {
    val details = raw"\d+".r.findAllIn(line).toArray.map(_.toInt)
    Reindeer(details(0), details(1), details(2))
})

def totalDistance(reindeer: Reindeer, totalTime: Int): Int = {
    val restartTime = reindeer.durationTime + reindeer.restTime
    var totalRuns = totalTime / restartTime
    val remainingTime = totalTime % restartTime
    if remainingTime >= reindeer.durationTime then totalRuns += 1
    return reindeer.speed * reindeer.durationTime * totalRuns
}

def distanceTravelledEverySecond(reindeer: Reindeer, totalTime: Int): Array[Int] = {
    val restartTime = reindeer.durationTime + reindeer.restTime
    val result = Array.tabulate(totalTime) { i => if (i % restartTime < reindeer.durationTime) 1 else 0 }
    for i <- 1 until totalTime do result(i) += result(i - 1)
    return result.map(_ * reindeer.speed)
}

def evaluatorOne(reindeers: List[Reindeer]): Int = {
    return reindeers.map(it => totalDistance(it, raceTotalTime)).max
}

def evaluatorTwo(reindeers: List[Reindeer]): Int = {
    val raceTimeStamps = reindeers.map(it => distanceTravelledEverySecond(it, raceTotalTime)).transpose
    val pointsCollection = Array.ofDim[Int](reindeers.length)

    raceTimeStamps.foreach(raceTimeStamp => {
        val maxDistance = raceTimeStamp.max
        
        val players = raceTimeStamp.zipWithIndex.collect {
            case (distance, playerNo) if distance == maxDistance => playerNo
        }

        for i <- players do pointsCollection(i) += 1
    })

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