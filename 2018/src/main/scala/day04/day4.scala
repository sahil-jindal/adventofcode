package day04

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer
import com.github.nscala_time.time.Imports._

case class Day(guard: Int, sleep: List[Boolean]) 
case class Guard(id: Int, sleepByMin: List[Int])

def groupLogs(input: List[String]): List[List[String]] = {
    return input.foldLeft(List.empty[List[String]]) {
        case (Nil, line) if line.contains("Guard") => List(List(line))
        case (acc, line) if line.contains("Guard") => acc :+ List(line)
        case (acc, line) => acc.init :+ (acc.last :+ line)
    }
}

def extractMinute(input: String): Int = {
    val timestamp = raw"\[(.*)\]".r.findFirstMatchIn(input).get.group(1)
    return DateTimeFormat.forPattern("yyyy-MM-dd HH:mm").parseDateTime(timestamp).minuteOfHour.get
}

def parseGroup(lines: List[String]): Day = {
    val (first, remaining) = (lines.head, lines.tail)

    val guard = raw"Guard #(\d+) begins shift".r.findFirstMatchIn(first).get.group(1).toInt

    val sleep = Array.fill(60)(false)

    for (List(asleep, wakeUp) <- remaining.grouped(2)) { 
        val start = extractMinute(asleep) 
        val end = extractMinute(wakeUp)
        for (min <- start until end) { sleep(min) = true }
    }

    return Day(guard, sleep.toList)
}

def parseInput(input: List[String]) = groupLogs(input.sorted).map(parseGroup)

def preComputeRecords(records: List[Day]): List[Guard]  = {
    return records.groupMap(_.guard)(_.sleep).toList.map { case (id, sleepDays) =>
        Guard(id, sleepDays.transpose.map(_.count(identity)))
    }
}

def solver(guards: List[Guard], maxSleepFunc: Guard => Int): Int = {
    val Guard(id, sleepByMin) = guards.maxBy(maxSleepFunc)
    val min = sleepByMin.zipWithIndex.maxBy(_._1)._2
    return id * min
}

def evaluatorOne(guards: List[Guard]): Int = solver(guards, it => it.sleepByMin.sum)
def evaluatorTwo(guards: List[Guard]): Int = solver(guards, it => it.sleepByMin.max)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

@main
def hello(): Unit = {
    readLinesFromFile("day04.txt") match {
        case Success(lines) => {
            val input = preComputeRecords(parseInput(lines))
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}