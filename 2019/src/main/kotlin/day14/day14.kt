package day14

import java.io.File
import java.util.LinkedList
import java.util.Queue

data class Chemical(val name: String, val amount: Long)

fun parseReagent(st: String): Chemical {
    val parts = st.split(" ")
    return Chemical(parts[1], parts[0].toLong())
}

fun parseInput(productionRules: List<String>): (Long) -> Long {
    val reactions = productionRules.associate { rule ->
        val (inputPart, outputPart) = rule.split(" => ")
        val inputs = inputPart.split(", ").map(::parseReagent)
        val output = parseReagent(outputPart)
        output.name to (output to inputs)
    }

    return { fuel ->
        var ore = 0L
        val inventory = reactions.keys.associateWith { 0L }.toMutableMap()

        val productionList: Queue<Chemical> = LinkedList()
        productionList.add(Chemical("FUEL", fuel))

        while (productionList.isNotEmpty()) {
            val (chemical, amount) = productionList.remove()

            if (chemical == "ORE") {
                ore += amount
            } else {
                val (output, inputs) = reactions[chemical]!!

                val useFromInventory = minOf(amount, inventory[chemical]!!)
                val remainingAmount = amount - useFromInventory
                inventory[chemical] = inventory[chemical]!! - useFromInventory

                if (remainingAmount > 0) {
                    val multiplier = (remainingAmount.toDouble() / output.amount).toLong().let {
                        if (remainingAmount % output.amount > 0) it + 1 else it
                    }

                    inventory[chemical] = maxOf(0, multiplier * output.amount - remainingAmount)

                    for ((reagent, qty) in inputs) {
                        productionList.add(Chemical(reagent, qty * multiplier))
                    }
                }
            }
        }

        ore
    }
}

fun evaluatorOne(input: List<String>): Long = parseInput(input)(1)

fun evaluatorTwo(input: List<String>): Long {
    val oreForFuel = parseInput(input)

    val ore = 1_000_000_000_000L
    var fuel = 1L

    while (true) {
        val newFuel = (ore.toDouble() / oreForFuel(fuel) * fuel).toLong()
        if (newFuel == fuel) return newFuel
        fuel = newFuel
    }
}

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day14.txt")
        .onSuccess {
            println("Part One: ${evaluatorOne(it)}")
            println("Part Two: ${evaluatorTwo(it)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}