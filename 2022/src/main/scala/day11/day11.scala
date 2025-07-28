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
    var inspectedItems: Long = 0
)

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def parseMonkey(input: List[String]): Monkey = {
    val items = Queue.from(input(1).trim().stripPrefix("Starting items: ").split(", ").map(_.toLong))
    val Array(operand, b) = input(2).trim().stripPrefix("Operation: new = old ").split(" ")

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

    val mod = input(3).trim().stripPrefix("Test: divisible by ").toInt
    val passToMonkeyIfDivides = input(4).trim().stripPrefix("If true: throw to monkey ").toInt
    val passToMonkeyOtherwise = input(5).trim().stripPrefix("If false: throw to monkey ").toInt

    return Monkey(items, operation, mod, passToMonkeyIfDivides, passToMonkeyOtherwise)
}

def parseMonkeys(input: List[String]) = groupLines(input).map(parseMonkey)

def run(monkeys: List[Monkey], rounds: Int, updateWorryLevel: Long => Long): List[Long] = {
    for (_ <- 1 to rounds; monkey <- monkeys) {
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

    return monkeys.map(_.inspectedItems)
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

def evaluatorOne(input: List[String]): Long = {
    val monkeys = parseMonkeys(input)
    return getMonkeyBusinessLevel(run(monkeys, 20, _ / 3))
}

def evaluatorTwo(input: List[String]): Long = {
    val monkeys = parseMonkeys(input)
    val mod = monkeys.map(_.mod).product
    return getMonkeyBusinessLevel(run(monkeys, 10000, _ % mod))
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