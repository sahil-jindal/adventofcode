package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer

sealed trait Op
case object Noop extends Op
case class Add(num: Int) extends Op

case class Signal(cycle: Int, x: Int)

def parseInput(input: List[String]) = input.collect {
    case "noop" => Noop
    case s"addx $num" => Add(num.toInt)
}

def getSignal(operations: List[Op]): List[Signal] = {
    var (cycle, x) = (1, 1)
    val res = ListBuffer.empty[Signal]

    for (op <- operations) {
        op match {
            case Noop => {
                res += Signal(cycle, x)
                cycle += 1
            }
            case Add(num) => {
                res += Signal(cycle, x)
                res += Signal(cycle + 1, x)
                x += num
                cycle += 2
            }
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
            val input = getSignal(parseInput(lines))
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two:\n${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}