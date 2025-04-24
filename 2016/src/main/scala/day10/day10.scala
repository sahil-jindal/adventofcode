package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map, ListBuffer}

sealed trait Destination
case class Bot(id: Int) extends Destination
case class Output(id: Int) extends Destination

def parseInstructions(input: List[String]): (Map[Int, ListBuffer[Int]], Map[Int, (Destination, Destination)]) = {
    val valuePattern = """value (\d+) goes to bot (\d+)""".r
    val botPattern = """bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)""".r

    val initialValues = Map.empty[Int, ListBuffer[Int]]
    val rules = Map.empty[Int, (Destination, Destination)]

    for (line <- input) {
        line match {
            case valuePattern(value, bot) => {
                val botId = bot.toInt
                initialValues.getOrElseUpdate(botId, ListBuffer.empty) += value.toInt
            }
            case botPattern(botId, lowType, lowId, highType, highId) => {
                val lowDest = if (lowType == "bot") Bot(lowId.toInt) else Output(lowId.toInt)
                val highDest = if (highType == "bot") Bot(highId.toInt) else Output(highId.toInt)
                rules(botId.toInt) = (lowDest, highDest)
            }
            case _ => throw new IllegalArgumentException(s"Cannot parse instruction: $line")
        }
    }

    return (initialValues, rules)
}

def solve(input: List[String], targetLow: Int = 17, targetHigh: Int = 61): (Int, Int) = {
    val (initialValues, rules) = parseInstructions(input)

    val botValues = Map.empty[Int, ListBuffer[Int]]
    val outputValues = Map.empty[Int, ListBuffer[Int]]
    var targetBot = -1

    for ((botId, values) <- initialValues) {
        botValues(botId) = values
    }

    var madeProgress = true

    while (madeProgress) {

        madeProgress = false
        
        for ((botId, values) <- botValues.toMap if values.length == 2) {

            madeProgress = true
            
            val sortedValues = values.sorted
            val lowValue = sortedValues.head
            val highValue = sortedValues.last
            
            if (lowValue == targetLow && highValue == targetHigh) {
                targetBot = botId
            }
            
            val (lowDest, highDest) = rules(botId)

            botValues(botId) = ListBuffer.empty

            lowDest match {
                case Bot(id) => botValues.getOrElseUpdate(id, ListBuffer.empty) += lowValue
                case Output(id) => outputValues.getOrElseUpdate(id, ListBuffer.empty) += lowValue
            }
            
            highDest match {
                case Bot(id) => botValues.getOrElseUpdate(id, ListBuffer.empty) += highValue
                case Output(id) => outputValues.getOrElseUpdate(id, ListBuffer.empty) += highValue
            }
        }
    }

    val outputProduct = outputValues(0).head * outputValues(1).head * outputValues(2).head

    return (targetBot, outputProduct)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day10.txt") match {
        case Success(lines) => {
            val (part1, part2) = solve(lines)
            println(s"Part 1: $part1")
            println(s"Part 2: $part2")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}