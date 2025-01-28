package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

val raceTotalTime = 2503

class Reindeer(
    val speed: Int, 
    val durationTime: Int, 
    val restTime: Int
)

def parseInput(line: String) = {
    val details = raw"\d+".r.findAllIn(line).toArray.map(_.toInt)
    Reindeer(details(0), details(1), details(2))
}

def totalDistance(reindeer: Reindeer, totalTime: Int) = {
    val restartTime = reindeer.durationTime + reindeer.restTime
    var totalRuns = totalTime / restartTime
    val remainingTime = totalTime % restartTime

    val oneMoreRuns = if remainingTime >= reindeer.durationTime then 1 else 0
    totalRuns += oneMoreRuns

    reindeer.speed * reindeer.durationTime * totalRuns
}

def distanceTravelledEverySecond(reindeer: Reindeer, totalTime: Int) = {
    val restartTime = reindeer.durationTime + reindeer.restTime
    val result = Seq.tabulate(totalTime)(i => if (i % restartTime < reindeer.durationTime) 1 else 0).toArray
    for i <- 1 until totalTime do result(i) += result(i - 1)
    result.map(it => it * reindeer.speed)
}

def evaluatorOne(reindeers: Array[Reindeer]) = {
    reindeers.map(it => totalDistance(it, raceTotalTime)).max
}

def evaluatorTwo(reindeers: Array[Reindeer]) = {
    val raceTimeStamps = reindeers.map(it => distanceTravelledEverySecond(it, raceTotalTime)).transpose
    val pointsCollection = Array.ofDim[Int](reindeers.length)

    raceTimeStamps.foreach(raceTimeStamp => {
        val maxDistance = raceTimeStamp.max
        
        val players = raceTimeStamp.zipWithIndex.collect {
            case (distance, playerNo) if distance == maxDistance => playerNo
        }

        for i <- players do pointsCollection(i) += 1
    })

    pointsCollection.max
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

@main
def hello(): Unit =
    readLinesFromFile("day14.txt") match
        case Success(lines) => {
            val reindeers = lines.map(parseInput).toArray
            println(s"Part One: ${evaluatorOne(reindeers)}")
            println(s"Part Two: ${evaluatorTwo(reindeers)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }