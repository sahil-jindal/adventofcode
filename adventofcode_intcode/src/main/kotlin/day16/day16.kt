package day16

import java.io.File
import kotlin.math.abs
import java.math.BigInteger

fun parseInput(input: String): IntArray {
    return input.map { it.toString().toInt() }.toIntArray()
}

fun pattern(digit: Int): Sequence<Int> = sequence {
    val times = digit + 1
    while (true) {
        listOf(0, 1, 0, -1).forEach { item ->
            repeat(times) { yield(item) }
        }
    }
}

fun fft(digits: IntArray): IntArray {
    return digits.indices.map { i ->
        val pattern = pattern(i).drop(1).take(digits.size).toList()
        val dotProduct = digits.zip(pattern) { a, b -> a * b }.sum()
        abs(dotProduct) % 10
    }.toIntArray()
}

fun evaluatorOne(input: String): String {
    var digits = parseInput(input)
    repeat(100) { digits = fft(digits) }
    return digits.take(8).joinToString("")
}

fun evaluatorTwo(input: String): String {
    val xs = parseInput(input)
    var res = ""

    val t = input.substring(0, 7).toInt()

    val height = 8
    val width = input.length * 10000 - t

    val bijMods = IntArray(width + 1)
    var bij = BigInteger.ONE

    for (j in 1..width) {
        bijMods[j] = (bij % BigInteger.TEN).toInt()
        bij = bij * BigInteger.valueOf((j + 99).toLong()) / BigInteger.valueOf(j.toLong())
    }

    for (i in 1..height) {
        var s = 0

        for (j in i..width) {
            val x = xs[(t + j - 1) % input.length]
            s += x * bijMods[j - i + 1]
        }

        res += (s % 10).toString()
    }

    return res
}

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day16.txt")
        .onSuccess {
            val line = it.first()
            println("Part One: ${evaluatorOne(line)}")
            println("Part Two: ${evaluatorTwo(line)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}