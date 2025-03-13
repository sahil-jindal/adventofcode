package day11

import intcode.IntCodeMachine
import java.io.File

data class Point(val y: Int, val x: Int)
data class Direction(val dy: Int, val dx: Int)

fun run(input: String, startColor: Long): Map<Point, Long> {
    val mtx = mutableMapOf<Point, Long>()

    var pos = Point(0, 0)
    var dir = Direction(-1, 0)

    mtx[pos] = startColor

    val icm = IntCodeMachine(input)

    while (true) {
        val output = icm.run(mtx.getOrDefault(pos, 0))
        if (icm.halted()) {
            return mtx
        }

        mtx[pos] = output[0]

        dir = when (output[1]) {
            0L -> Direction(-dir.dx, dir.dy)
            1L -> Direction(dir.dx, -dir.dy)
            else -> throw Exception()
        }

        pos = Point(pos.y + dir.dy, pos.x + dir.dx)
    }
}

fun evaluatorOne(input: String): Int = run(input, 0).count()

fun evaluatorTwo(input: String): String {
    val dict = run(input, 1)

    val ys = dict.keys.map { it.y }
    val xs = dict.keys.map { it.x }

    val ymin = ys.min()
    val ymax = ys.max()
    val xmin = xs.min()
    val xmax = xs.max()

    var st = ""

    for (y in ymin..ymax) {
        for (x in xmin..xmax) {
            st += " #"[dict.getOrDefault(Point(y, x), 0).toInt()]
        }

        st += "\n"
    }

    return st
}

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day11.txt")
        .onSuccess {
            val line = it.first()
            println("Part One: ${evaluatorOne(line)}")
            println("Part Two:\n${evaluatorTwo(line)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}