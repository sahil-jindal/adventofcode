package day07

import java.io.File
import intcode.IntCodeMachine

fun <T> permutations(list: List<T>): List<List<T>> {
    if (list.size <= 1) return listOf(list)
    return list.flatMap { item -> permutations(list - item).map { sublist -> listOf(item) + sublist } }
}

fun execAmps(amps: List<IntCodeMachine>, prgId: List<Int>, loop: Boolean): Long {
    amps.forEachIndexed { i, amp ->
        amp.reset()
        amp.inputQueue.add(prgId[i].toLong())
    }

    var data = longArrayOf(0L)

    while (true) {
        for (amp in amps) {
            data = amp.run(*data).toLongArray()
        }
        if (amps.all { it.halted() }) {
            return data.last()
        }
        if (!loop) {
            data = longArrayOf(0L)
        }
    }
}

fun solve(prg: String, loop: Boolean, prgIds: List<Int>): Long {
    val amps = List(5) { IntCodeMachine(prg) }
    return permutations(prgIds).maxOf { execAmps(amps, it, loop) }
}

fun evaluatorOne(prg: String): Long = solve(prg, false, listOf(0, 1, 2, 3, 4))
fun evaluatorTwo(prg: String): Long = solve(prg, true, listOf(5, 6, 7, 8, 9))

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day07.txt")
        .onSuccess {
            val line = it.first()
            println("Part One: ${evaluatorOne(line)}")
            println("Part Two: ${evaluatorTwo(line)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}