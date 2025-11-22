package day25

import intcode.ImmutableIntCodeMachine
import java.io.File

fun evaluator(program: String): String {
    val dangerousItems = setOf(
        "infinite loop",
        "molten lava",
        "photons",
        "escape pod",
        "giant electromagnet"
    )

    var machine = ImmutableIntCodeMachine(program)
    val items = mutableListOf<String>()
    val path = mutableListOf<String>()

    // Automated exploration to collect safe items and reach the checkpoint
    while (true) {
        val (newMachine, output) = machine.run(*longArrayOf())
        machine = newMachine
        val outputText = output.toAscii()

        if (outputText.contains("== Security Checkpoint ==")) {
            break
        }

        // Check for game over due to taking a dangerous item
        if (outputText.contains("You lose.")) {
            throw IllegalStateException("Picked up a dangerous item.")
        }

        val currentItems = parseItems(outputText).filterNot { it in dangerousItems }
        currentItems.forEach { item ->
            if (!items.contains(item)) {
                machine = machine.run("take $item").first
                items.add(item)
            }
        }

        val directions = parseDirections(outputText)
        var moved = false
        for (dir in directions) {
            val opposite = when (dir) {
                "north" -> "south"
                "south" -> "north"
                "east" -> "west"
                "west" -> "east"
                else -> continue
            }
            if (path.isEmpty() || path.last() != opposite) {
                path.add(dir)
                machine = machine.run(dir).first
                moved = true
                break
            }
        }

        if (!moved && path.isNotEmpty()) {
            val lastDir = path.removeLast()
            val opposite = when (lastDir) {
                "north" -> "south"
                "south" -> "north"
                "east" -> "west"
                "west" -> "east"
                else -> error("Invalid direction")
            }
            machine = machine.run(opposite).first
        } else if (!moved) {
            break
        }
    }

    // Test all subsets of collected items
    val baseMachine = machine
    for (mask in 1 until (1 shl items.size)) {
        val subset = mutableListOf<String>()
        for (i in items.indices) {
            if ((mask and (1 shl i)) != 0) {
                subset.add(items[i])
            }
        }
        val dropCommands = items.filterNot { subset.contains(it) }.map { "drop $it" }
        val commands = dropCommands + "south"

        val (_, output) = baseMachine.run(*commands.toTypedArray())
        val outputText = output.toAscii()

        if (outputText.contains("typing the password")) {
            return outputText.lines()
                .firstOrNull { it.contains(Regex("\\d+")) }
                ?.let { Regex("\\d+").find(it)?.value }
                ?: continue
        }
    }

    error("Password not found")
}

private fun parseDirections(outputText: String): List<String> {
    val directions = mutableListOf<String>()
    var inDoorsSection = false
    for (line in outputText.lines()) {
        when {
            line.startsWith("Doors here lead:") -> inDoorsSection = true
            inDoorsSection && line.startsWith("- ") -> directions.add(line.substring(2))
            inDoorsSection && line.isBlank() -> inDoorsSection = false
        }
    }
    return directions
}

private fun parseItems(outputText: String): List<String> {
    val items = mutableListOf<String>()
    var inItemsSection = false
    for (line in outputText.lines()) {
        when {
            line.startsWith("Items here:") -> inItemsSection = true
            inItemsSection && line.startsWith("- ") -> items.add(line.substring(2))
            inItemsSection && line.isBlank() -> inItemsSection = false
        }
    }
    return items
}

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day25.txt")
        .onSuccess {
            println("Part One: ${evaluator(it.first())}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}