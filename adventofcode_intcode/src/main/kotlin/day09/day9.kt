package day09

import java.io.File
import intcode.IntCodeMachine

fun evaluatorOne(input: String): Long = IntCodeMachine(input).run(1).single()
fun evaluatorTwo(input: String): Long = IntCodeMachine(input).run(2).single()

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day09.txt")
        .onSuccess {
            val line = it.first()
            println("Part One: ${evaluatorOne(line)}")
            println("Part Two: ${evaluatorTwo(line)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}