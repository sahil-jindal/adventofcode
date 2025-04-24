package day11

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Queue

case class Monkey(
    val items: Queue[Long],
    val operation: Long => Long,
    val mod: Int,
    val passToMonkeyIfDivides: Int,
    val passToMonkeyOtherwise: Int,
    var inspectedItems: Int = 0
)

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def parseMonkey(input: List[String]): Monkey = {
    val startingItems = input(1).split(": ")(1).split(", ").map(_.toLong)
    val items = Queue(startingItems*)

    val Array(_, operand, b) = input(2).split(" = ")(1).split(" ")

    var operation: Long => Long = identity

    if (operand == "+") {
        operation = old => old + b.toLong
    } else if (operand == "*") {
        if (b == "old") {
            operation = old => old * old
        } else {
            operation = old => old * b.toLong
        }
    }

    val mod = input(3).split(": ")(1).split(" ")(2).toInt
    val passToMonkeyIfDivides = input(4).split(": ")(1).split(" ")(3).toInt
    val passToMonkeyOtherwise = input(5).split(": ")(1).split(" ")(3).toInt

    return Monkey(items, operation, mod, passToMonkeyIfDivides, passToMonkeyOtherwise)
}

def parseMonkeys(input: List[String]): Array[Monkey] = groupLines(input).map(parseMonkey).toArray

def run(rounds: Int, monkeys: Array[Monkey], updateWorryLevel: Long => Long): Unit = {
    for (_ <- 1 to rounds) {
        for (monkey <- monkeys) {
            while (monkey.items.nonEmpty) {
                monkey.inspectedItems += 1

                var item = monkey.items.dequeue()
                item = monkey.operation(item)
                item = updateWorryLevel(item)

                val target = if (item % monkey.mod == 0) {
                    monkey.passToMonkeyIfDivides
                } else {
                    monkey.passToMonkeyOtherwise
                }

                monkeys(target).items.enqueue(item)
            }
        }
    }
}

def getMonkeyBusinessLevel(monkeys: Array[Monkey]): Long = {
    var topMost: Option[Long] = None
    var secondMost: Option[Long] = None

    for(monkey <- monkeys) {
        val value = monkey.inspectedItems.toLong

        if (topMost.isEmpty || value >= topMost.get) {
            secondMost = topMost
            topMost = Some(value)
        } else if (secondMost.isEmpty || value >= secondMost.get) {
            secondMost = Some(value)
        }
    }

    return topMost.get * secondMost.get
}

def evaluatorOne(input: List[String]): Long = {
    val monkeys = parseMonkeys(input)
    run(20, monkeys, _ / 3)
    getMonkeyBusinessLevel(monkeys)
}

def evaluatorTwo(input: List[String]): Long = {
    val monkeys = parseMonkeys(input)
    val mod = monkeys.map(_.mod).product
    run(10000, monkeys, _ % mod)
    getMonkeyBusinessLevel(monkeys)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day11.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}