package day01

import java.io.File

fun parseInput(lines: List<String>) = lines.map { it.toInt() }

fun helper(weights: List<Int>) = weights.map { it / 3 - 2 }.filter { it > 0 }

fun evaluatorOne(weights: List<Int>) = helper(weights).sum()

fun evaluatorTwo(weights: List<Int>): Int {
    var fuels = helper(weights)
    var total = 0

    while (fuels.isNotEmpty()) {
        total += fuels.sum()
        fuels = helper(fuels)
    }

    return total
}

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day01.txt")
        .onSuccess {
            val weights = parseInput(it)
            println("Part One: ${evaluatorOne(weights)}")
            println("Part Two: ${evaluatorTwo(weights)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}