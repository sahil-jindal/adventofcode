package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{ListBuffer, Map => MutableMap}

sealed trait Destination
case class Bot(id: Int) extends Destination
case class Output(id: Int) extends Destination

val valuePattern = raw"value (\d+) goes to bot (\d+)".r
val botPattern = raw"bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)".r

def parseInstructions(input: List[String]): (Map[Int, ListBuffer[Int]], Map[Int, (Destination, Destination)]) = {
    val initialValues = MutableMap.empty[Int, ListBuffer[Int]]
    val rules = MutableMap.empty[Int, (Destination, Destination)]

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

    return (initialValues.toMap, rules.toMap)
}

def solve(input: List[String], targetLow: Int = 17, targetHigh: Int = 61): (Int, Int) = {
    val (initialValues, rules) = parseInstructions(input)

    val botValues = MutableMap.from(initialValues)

    val outputValues = MutableMap.empty[Int, ListBuffer[Int]]
    var targetBot = -1
    var madeProgress = true

    while (madeProgress) {

        madeProgress = false
        
        for ((botId, values) <- botValues.toMap if values.length == 2) {

            madeProgress = true
            
            val lowValue = values.min
            val highValue = values.max
            
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
            val (partOne, partTwo) = solve(lines)
            println(s"Part One: $partOne")
            println(s"Part Two: $partTwo")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}