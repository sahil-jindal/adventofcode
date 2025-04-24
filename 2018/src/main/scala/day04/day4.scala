package day04

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer
import com.github.nscala_time.time.Imports._

case class Day(guard: Int, sleep: Array[Int]) {
    def totalSleep: Int = sleep.sum
}

def extractMinute(line: String): Int = {
    val timestamp = "\\[(.*)\\]".r.findFirstMatchIn(line).get.group(1)
    return DateTimeFormat.forPattern("yyyy-MM-dd HH:mm").parseDateTime(timestamp).minuteOfHour.get
}

def parseInput(input: List[String]): Seq[Day] = {
    val lines = input.sorted
    
    val result = ListBuffer.empty[Day]
    var iline = 0
    
    while (iline < lines.length) {
        val guard = "Guard #(\\d+) begins shift".r.findFirstMatchIn(lines(iline)).get.group(1).toInt
        iline += 1

        val sleep = Array.fill(60)(0)
        
        while (iline < lines.length && !lines(iline).contains("Guard")) {
            val start = extractMinute(lines(iline))
            iline += 1

            val end = extractMinute(lines(iline))
            iline += 1

            for (min <- start until end) { sleep(min) = 1 }
        }

        result += Day(guard, sleep)
    }

    return result.toSeq
}

def evaluatorOne(records: Seq[Day]): Int = {
    val grouped = records.groupBy(_.guard).view.mapValues { days =>
        val totalSleeps = days.map(_.totalSleep).sum
        val sleepByMin = Seq.tabulate(60) { minT => days.map(_.sleep(minT)).sum }
        (totalSleeps, sleepByMin)
    }

    val (guard, (_, sleepByMin)) = grouped.maxBy(_._2._1)
    val min = sleepByMin.zipWithIndex.maxBy(_._1)._2
    return guard * min
}

def evaluatorTwo(records: Seq[Day]): Int = {
    val grouped = records.groupBy(_.guard).view.mapValues { days =>
        val sleepByMin = Seq.tabulate(60) { minT => days.map(_.sleep(minT)).sum }
        sleepByMin
    }

    val (guard, sleepByMin) = grouped.maxBy(_._2.max)
    val min = sleepByMin.zipWithIndex.maxBy(_._1)._2
    return guard * min
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day04.txt") match {
        case Success(lines) => {
            val records = parseInput(lines)
            println(s"Part One: ${evaluatorOne(records)}")
            println(s"Part Two: ${evaluatorTwo(records)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}