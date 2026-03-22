package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Queue

sealed trait Destination
case class Bot(id: Int) extends Destination
case class Output(id: Int) extends Destination

case class Node(
    low: Destination,
    high: Destination,
    var chip: Option[Int] = None,
)

val valuePattern = raw"value (\d+) goes to bot (\d+)".r
val botPattern = raw"bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)".r

def parseInstructions(input: List[String]): (List[(Destination, Int)], Map[Int, Node]) = {
    val initialTodos = input.collect { 
        case valuePattern(value, bot) => (Bot(bot.toInt), value.toInt)
    }
    
    val initialBots = input.collect {
        case botPattern(botId, lowType, lowId, highType, highId) => {
            val lowDest = if (lowType == "bot") Bot(lowId.toInt) else Output(lowId.toInt)
            val highDest = if (highType == "bot") Bot(highId.toInt) else Output(highId.toInt)
            botId.toInt -> Node(lowDest, highDest)
        }
    }

    return (initialTodos, initialBots.toMap)
}

def solve(input: List[String]): (Int, Int) = {
    val (initialTodos, bots) = parseInstructions(input)

    val todo = Queue.from(initialTodos)

    var partOne = Int.MaxValue
    var partTwo = 1

    while (todo.nonEmpty) {
        val (dest, value) = todo.dequeue()

        dest match {
            case Bot(index) => {
                bots.get(index).foreach { bot =>
                    if (bot.chip.isDefined) {
                        val previous = bot.chip.get

                        val min = previous.min(value)
                        val max = previous.max(value)

                        todo.enqueue((bot.low, min))
                        todo.enqueue((bot.high, max))

                        if (min == 17 && max == 61) {
                            partOne = index
                        }
                    } else {
                        bot.chip = Some(value)
                    }
                }
            }
            case Output(index) => {
                if (index <= 2) {
                    partTwo *= value
                }
            }
        }
    }

    (partOne, partTwo)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day10.txt") match {
        case Success(lines) => {
            val (partOne, partTwo) = solve(lines)
            println(s"Part One: $partOne")
            println(s"Part Two: $partTwo")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}