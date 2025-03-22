package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map, Queue}

case class Chemical(name: String, amount: Long)

def parseReagent(st: String): Chemical = {
    val parts = st.split(" ")
    return Chemical(parts(1), parts(0).toLong)
}

def parseInput(productionRules: List[String]): Long => Long = {
    val reactions = productionRules.map { rule =>
        val Array(inputPart, outputPart) = rule.split(" => ")
        val inputs = inputPart.split(", ").map(parseReagent).toList
        val output = parseReagent(outputPart)
        output.name -> (output, inputs)
    }.toMap

    return fuel => {
        var ore = 0L
        val inventory = Map(reactions.keys.map(_ -> 0L).toSeq*)
        val productionList = Queue(Chemical("FUEL", fuel))

        while (productionList.nonEmpty) {
            val Chemical(chemical, amount) = productionList.dequeue()

            if (chemical == "ORE") {
                ore += amount
            } else {
                val (output, inputs) = reactions(chemical)

                val useFromInventory = math.min(amount, inventory(chemical))
                val remainingAmount = amount - useFromInventory
                inventory(chemical) -= useFromInventory

                if (remainingAmount > 0) {
                    val multiplier = (remainingAmount.toDouble / output.amount).toLong match {
                        case m if remainingAmount % output.amount > 0 => m + 1
                        case m => m
                    }

                    inventory(chemical) = math.max(0, multiplier * output.amount - remainingAmount)

                    for (Chemical(reagent, qty) <- inputs) {
                        productionList.enqueue(Chemical(reagent, qty * multiplier))
                    }
                }
            }
        }

        ore
    }
}

def evaluatorOne(input: List[String]): Long = parseInput(input)(1)

def evaluatorTwo(input: List[String]): Long = {
    val oreForFuel = parseInput(input)

    val ore = 1_000_000_000_000L
    var fuel = 1L

    while (true) {
        val newFuel = (ore.toDouble / oreForFuel(fuel) * fuel).toLong
        if (newFuel == fuel) return newFuel
        fuel = newFuel
    }

    return fuel
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day14.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}