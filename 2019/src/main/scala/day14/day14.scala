package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Queue, Map => MutableMap}

case class Chemical(name: String, amount: Long)

type ChemicalReactions = Map[String, (Long, List[Chemical])]

def parseReagent(st: String): Chemical = {
    val Array(a, b) = st.split(" ")
    return Chemical(b, a.toLong)
}

def parseRules(rule: String) = {
    val Array(inputPart, outputPart) = rule.split(" => ")
    val inputs = inputPart.split(", ").map(parseReagent).toList
    val output = parseReagent(outputPart)
    output.name -> (output.amount, inputs)
}

def parseInput(input: List[String]) = input.map(parseRules).toMap

def helper(reactions: ChemicalReactions): Long => Long = {
    return fuel => {
        var ore = 0L
        val inventory = MutableMap.from(reactions.keys.map(_ -> 0L))
        val productionList = Queue(Chemical("FUEL", fuel))

        while (productionList.nonEmpty) {
            val Chemical(chemical, amount) = productionList.dequeue()

            if (chemical == "ORE") {
                ore += amount
            } else {
                val (outputAmmount, inputs) = reactions(chemical)

                val useFromInventory = math.min(amount, inventory(chemical))
                val remainingAmount = amount - useFromInventory
                inventory(chemical) -= useFromInventory

                if (remainingAmount > 0) {
                    var multiplier = remainingAmount / outputAmmount
                    if (remainingAmount % outputAmmount > 0) then multiplier += 1

                    inventory(chemical) = multiplier * outputAmmount - remainingAmount

                    for (Chemical(reagent, qty) <- inputs) {
                        productionList.enqueue(Chemical(reagent, qty * multiplier))
                    }
                }
            }
        }

        ore
    }
}

def evaluatorOne(input: ChemicalReactions): Long = helper(input)(1)

def evaluatorTwo(input: ChemicalReactions): Long = {
    val oreForFuel = helper(input)

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
            val input = parseInput(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}