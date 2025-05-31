package aoc2017.day18

import java.util.LinkedList
import java.io.File

fun parseInput(input: List<String>) = input.map { it.split(" ") }

fun evaluatorOne(input: List<List<String>>): Long {
    return MachineOne().execute(input).first { it != null }!!
}

fun evaluatorTwo(input: List<List<String>>): Int {
    val p0Input = LinkedList<Long>()
    val p1Input = LinkedList<Long>()

    val p0 = MachineTwo(0, p0Input, p1Input).execute(input).iterator()
    val p1 = MachineTwo(1, p1Input, p0Input).execute(input).iterator()

    while (p0.hasNext() && p1.hasNext()) {
        val state0 = p0.next()
        val state1 = p1.next()

        if (!state0.first && !state1.first) {
            return state1.second
        }
    }

    return -1
}

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/aoc2017/day18.txt")
        .onSuccess {
            val input = parseInput(it)
            println("Part One: ${evaluatorOne(input)}")
            println("Part Two: ${evaluatorTwo(input)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}