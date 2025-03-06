package day05

import java.io.File
import intcode.IntCodeMachine

fun evaluatorOne(input: String): Long = IntCodeMachine(input).run(1).last()
fun evaluatorTwo(input: String): Long = IntCodeMachine(input).run(5).last()

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day05.txt")
        .onSuccess {
            val line = it.first()
            println("Part One: ${evaluatorOne(line)}")
            println("Part Two: ${evaluatorTwo(line)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}