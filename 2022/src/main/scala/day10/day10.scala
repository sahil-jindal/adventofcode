package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer

case class Signal(cycle: Int, x: Int)

def getSignal(input: List[String]): List[Signal] = {
    var (cycle, x) = (1, 1)
    val res = ListBuffer.empty[Signal]

    for (line <- input) {
        val parts = line.split(" ")

        parts(0) match {
            case "noop" => {
                res += Signal(cycle, x)
                cycle += 1
            }
            case "addx" => {
                res += Signal(cycle, x)
                res += Signal(cycle + 1, x)
                x += parts(1).toInt
                cycle += 2
            }
            case _ => throw Exception()
        }
    }

    return res.toList
}

def evaluatorOne(signals: List[Signal]): Int = {
    val sample = (20 to 220 by 40)
    return signals.withFilter(it => sample.contains(it.cycle)).map(it => it.x * it.cycle).sum
}

def evaluatorTwo(signals: List[Signal]): String = {
    val sprites = signals.map { case Signal(cycle, x) => 
        var screenColumn = (cycle - 1) % 40
        if (x - screenColumn).abs < 2 then '#' else ' '    
    }

    return sprites.grouped(40).map(_.mkString).mkString("\n")
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day10.txt") match {
        case Success(lines) => {
            val input = getSignal(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two:\n${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}