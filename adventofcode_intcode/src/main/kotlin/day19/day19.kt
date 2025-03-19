package day19

import intcode.ImmutableIntCodeMachine
import java.io.File

fun detector(input: String): (Long, Long) -> Boolean {
    val icm = ImmutableIntCodeMachine(input)
    return { x, y -> icm.run(x, y).second[0] == 1L }
}

fun evaluatorOne(input: String): Int {
    val detect = detector(input)
    var count = 0

    for (x in 0L until 50L) {
        for (y in 0L until 50L) {
            if (detect(x, y)) {
                count += 1
            }
        }
    }

    return count
}

fun evaluatorTwo(input: String): Long {
    val detect = detector(input)

    var y = 100L
    var x = 0L

    while (true) {
        // Find the leftmost affected point at this y
        while (!detect(x, y)) x++

        // Check if a 100x100 square fits with top-left at (x, y-99)
        // Only valid if y >= 99
        if (y >= 99 && detect(x + 99, y) && detect(x, y - 99) && detect(x + 99, y - 99)) {
            return x * 10000L + (y - 99)
        }

        y++
        // We can optimize by not checking every x position
        // Since the beam expands linearly, we can start closer to the expected edge
        x = (x * y) / (y - 1) - 2 // Approximate the beam slope with some margin
        x = maxOf(x, 0) // Ensure x is non-negative
    }
}

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day19.txt")
        .onSuccess {
            val line = it.first()
            println("Part One: ${evaluatorOne(line)}")
            println("Part Two: ${evaluatorTwo(line)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}