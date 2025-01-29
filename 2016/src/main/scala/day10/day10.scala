package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{TreeSet, Map, Queue}

case class Bot(id: Int):
    val chips: TreeSet[Int] = TreeSet()
    var lowTarget: Option[(String, Int)] = None
    var highTarget: Option[(String, Int)] = None

    def receiveChip(value: Int): Unit = chips.add(value)
    def isReady: Boolean = chips.size == 2

    def process(): Option[(Int, Int, Int)] =
        if isReady then
            val low = chips.head
            val high = chips.last
            chips.clear()
            Some(id, low, high)
        else None

object BalanceBots:
    val botPattern = """bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)""".r
    val valuePattern = """value (\d+) goes to bot (\d+)""".r

    val bots = Map[Int, Bot]()
    val outputs = Map[Int, Int]()

    def getBot(id: Int): Bot = bots.getOrElseUpdate(id, Bot(id))

    def processInstructions(instructions: List[String]): Unit =
        val actionQueue = Queue[String]()

        instructions.foreach:
            case valuePattern(value, botId) => getBot(botId.toInt).receiveChip(value.toInt)
            case instr => actionQueue.enqueue(instr)

        while actionQueue.nonEmpty do
            val instr = actionQueue.dequeue()
            
            instr match
                case botPattern(botId, lowType, lowId, highType, highId) =>
                    val bot = getBot(botId.toInt)
                    
                    if bot.isReady then
                        val (id, low, high) = bot.process().get
                    
                        if (low == 17 && high == 61) || (low == 61 && high == 17) then
                            println(s"Bot responsible: $id")
                    
                        if lowType == "bot" then getBot(lowId.toInt).receiveChip(low)
                        else outputs(lowId.toInt) = low
                        if highType == "bot" then getBot(highId.toInt).receiveChip(high)
                        else outputs(highId.toInt) = high
                    else
                        actionQueue.enqueue(instr) // Retry later
                case _ =>

        if outputs.contains(0) && outputs.contains(1) && outputs.contains(2) then
            val product = outputs(0) * outputs(1) * outputs(2)
            println(s"Product of outputs 0, 1, and 2: $product")
        else
            println("Outputs 0, 1, and 2 are not all available.")

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("day10.txt") match
        case Success(lines) => {
            BalanceBots.processInstructions(lines)
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
