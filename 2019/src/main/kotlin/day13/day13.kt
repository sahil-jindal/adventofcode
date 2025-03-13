package day13

import intcode.IntCodeMachine
import java.io.File
import kotlin.math.sign

fun evaluatorOne(input: String): Int = IntCodeMachine(input).run().chunked(3).count { it[2] == 2L }

fun evaluatorTwo(input: String): Int {
    val icm = IntCodeMachine(input)
    icm.memory[0] = 2

    var score = 0
    var icolBall = -1
    var icolPaddle = -1
    var dir = 0L

    while (true) {
        val output = icm.run(dir)
        for (chuck in output.chunked(3)) {
            val (x, y, block) = chuck
            if (x == -1L && y == 0L) {
                score = block.toInt()
            }

            if (block == 3L) {
                icolPaddle = x.toInt()
            } else if (block == 4L) {
                icolBall = x.toInt()
            }
        }

        if (icm.halted()) break

        dir = (icolBall - icolPaddle).sign.toLong()
    }

    return score
}

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day13.txt")
        .onSuccess {
            val line = it.first()
            println("Part One: ${evaluatorOne(line)}")
            println("Part Two: ${evaluatorTwo(line)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}