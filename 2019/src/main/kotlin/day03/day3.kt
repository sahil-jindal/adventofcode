package day03

import java.io.File
import kotlin.math.abs

data class Quadruple<A, B, C, D>(val first: A, val second: B, val third: C, val fourth: D)

fun trace(path: String): Map<Pair<Int, Int>, Int> {
    val res = mutableMapOf<Pair<Int, Int>, Int>()
    var (y, x, distance) = Triple(0, 0, 0)

    for (step in path.split(",")) {
        val (dy, dx) = when (step[0]) {
            'U' -> -1 to 0
            'D' -> 1 to 0
            'R' -> 0 to -1
            'L' -> 0 to 1
            else -> throw IllegalArgumentException()
        }

        repeat(step.substring(1).toInt()) {
            y += dy
            x += dx
            distance++
            res.putIfAbsent(y to x, distance)
        }
    }

    return res
}

fun solve(paths: List<String>, distance: (Quadruple<Int, Int, Int, Int>) -> Int): Int {
    val trace1 = trace(paths[0])
    val trace2 = trace(paths[1])

    return trace1.keys.filter { it in trace2 }
        .minOf { distance(Quadruple(it.first, it.second, trace1[it]!!, trace2[it]!!)) }
}

fun evaluatorOne(input: List<String>): Int = solve(input) { abs(it.first) + abs(it.second) }
fun evaluatorTwo(input: List<String>): Int = solve(input) { it.third + it.fourth }

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day03.txt")
        .onSuccess {
            println("Part One: ${evaluatorOne(it)}")
            println("Part Two: ${evaluatorTwo(it)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}