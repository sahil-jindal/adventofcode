package day11

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Queue

case class Monkey(
    val id: Int,
    val mod: Int,
    val operation: Long => Long,
    val chooseMonkeytoPass: Long => Int,
    val items: Queue[Long] = Queue.empty
)

type Pair = (List[Long], Monkey)

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def parseMonkey(input: List[String]): Pair = {
    val monkeyId = input(0).trim().stripPrefix("Monkey ").stripSuffix(":").toInt
    val items = input(1).trim().stripPrefix("Starting items: ").split(", ").map(_.toLong).toList
    val line = input(2).trim().stripPrefix("Operation: new = ")

    val operation: Long => Long = line match {
        case s"old * old" => old => old * old
        case s"old * $b" => old => old * b.toLong
        case s"old + $b" => old => old + b.toLong
        case other => identity
    }

    val mod = input(3).trim().stripPrefix("Test: divisible by ").toInt
    val passToMonkeyIfDivides = input(4).trim().stripPrefix("If true: throw to monkey ").toInt
    val passToMonkeyOtherwise = input(5).trim().stripPrefix("If false: throw to monkey ").toInt

    val decideWhichMonkeytoPass: Long => Int = { item =>
        if (item % mod == 0) then passToMonkeyIfDivides else passToMonkeyOtherwise
    }

    return (items, Monkey(monkeyId, mod, operation, decideWhichMonkeytoPass))
}

def parseMonkeys(input: List[String]) = groupLines(input).map(parseMonkey).toVector

def run(input: Vector[Pair], rounds: Int, updateWorryLevel: Long => Long): List[Long] = {
    val monkeys = input.map { case (numbers, monkey) => 
        monkey.copy(items = Queue.from(numbers))    
    }

    val inspectedItemsCount = Array.ofDim[Long](monkeys.size)
    
    for (_ <- 1 to rounds; monkey <- monkeys) {
        while (monkey.items.nonEmpty) {
            inspectedItemsCount(monkey.id) += 1

            var item = monkey.items.dequeue()
            item = monkey.operation(item)
            item = updateWorryLevel(item)

            val target = monkey.chooseMonkeytoPass(item)

            monkeys(target).items.enqueue(item)
        }
    }

    return inspectedItemsCount.toList
}

def getMonkeyBusinessLevel(monkeyTransfers: List[Long]): Long = {
    var topMost: Option[Long] = None
    var secondMost: Option[Long] = None

    for (value <- monkeyTransfers) {
        if (topMost.isEmpty || value >= topMost.get) {
            secondMost = topMost
            topMost = Some(value)
        } else if (secondMost.isEmpty || value >= secondMost.get) {
            secondMost = Some(value)
        }
    }

    return topMost.get * secondMost.get
}

def evaluatorOne(input: Vector[Pair]): Long = {
    return getMonkeyBusinessLevel(run(input, 20, _ / 3))
}

def evaluatorTwo(input: Vector[Pair]): Long = {
    val mod = input.map(_._2.mod).product
    return getMonkeyBusinessLevel(run(input, 10000, _ % mod))
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day11.txt") match {
        case Success(lines) => {
            val input = parseMonkeys(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}