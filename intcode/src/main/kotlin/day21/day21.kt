package day21

import intcode.IntCodeMachine
import java.io.File

fun evaluatorOne(input: String): Long {
    // J = (¬A ∨ ¬B ∨ ¬C) ∧ D i.e. jump if no road ahead, but we can continue from D
    return IntCodeMachine(input).run("OR A T", "AND B T", "AND C T", "NOT T J", "AND D J", "WALK").last()
}

fun evaluatorTwo(input: String): Long {
    // J = (¬A ∨ ¬B ∨ ¬C) ∧ D ∧ (H ∨ E) i.e. same as part 1, but also check that D is not a dead end
    return IntCodeMachine(input).run("OR A T", "AND B T", "AND C T", "NOT T J", "AND D J", "OR H T", "OR E T", "AND T J", "RUN").last()
}

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day21.txt")
        .onSuccess {
            val line = it.first()
            println("Part One: ${evaluatorOne(line)}")
            println("Part Two: ${evaluatorTwo(line)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}