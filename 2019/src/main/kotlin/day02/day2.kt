package day02

import java.io.File
import intcode.IntCodeMachine

fun execIntCode(icm: IntCodeMachine, noun: Long, verb: Long): Long {
    icm.reset()
    icm.memory[1] = noun
    icm.memory[2] = verb
    icm.run()
    return icm.memory[0]
}

fun evaluatorOne(line: String) = execIntCode(IntCodeMachine(line), 12, 2)

fun evaluatorTwo(line: String): Int {
    val icm = IntCodeMachine(line)
    var sum = 0

    while(true) {
        for(verb in 0..sum) {
            val noun = sum - verb
            val res = execIntCode(icm, noun.toLong(), verb.toLong())
            if (res.toInt() == 19690720) return 100 * noun + verb
        }
        sum++
    }
}

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day02.txt")
        .onSuccess {
            val line = it.first()
            println("Part One: ${evaluatorOne(line)}")
            println("Part Two: ${evaluatorTwo(line)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}