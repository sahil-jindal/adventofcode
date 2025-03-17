package day17

import intcode.IntCodeMachine
import java.io.File

data class Point(val y: Int, val x: Int)
data class Direction(val dy: Int, val dx: Int)

val dir = listOf(Direction(-1, -1), Direction(-1, 0), Direction(-1, 1),
                Direction(0, -1), Direction(0, 0), Direction(0, 1),
                Direction(1, -1), Direction(1, 0), Direction(1, 1))

fun screenShot(input: String): List<String> {
    val output = IntCodeMachine(input).run()
    return output.toAscii().split("\n").filter { it.isNotBlank() }
}

fun generateRec(path: String, functions: List<String>): Sequence<Pair<List<Int>, List<String>>> = sequence {
    if (path.isEmpty()) yield(emptyList<Int>() to functions)

    functions.forEachIndexed { i, function ->
        if (path.startsWith(function)) {
            val pathT = path.removePrefix(function)
            generateRec(pathT, functions).forEach { (indices, funcs) ->
                yield(listOf(i) + indices to funcs)
            }
        }
    }

    if (functions.size < 3) {
        (1..path.length).forEach { length ->
            val function = path.substring(0, length)
            val functionsT = functions + function
            val idx = functions.size
            val pathT = path.removePrefix(function)
            generateRec(pathT, functionsT).forEach { (indices, funcs) ->
                yield(listOf(idx) + indices to funcs)
            }
        }
    }
}

fun compress(st: String): String {
    val steps = mutableListOf<String>()
    var l = 0

    for (ch in st) {
        if (l > 0 && ch != 'F') {
            steps.add(l.toString())
            l = 0
        }

        if (ch == 'R' || ch == 'L') steps.add(ch.toString())
        else l++
    }

    if (l > 0) steps.add(l.toString())

    return steps.joinToString(",")
}

fun generatePrograms(path: String): Sequence<String> {
    return generateRec(path, emptyList()).mapNotNull { (indices, functions) ->
        val compressed = functions.map { compress(it) }
        if (indices.size <= 20 && compressed.all { it.length <= 20 }) {
            val main = indices.joinToString(",") { "ABC"[it].toString() }
            "$main\n${compressed[0]}\n${compressed[1]}\n${compressed[2]}\nn\n"
        } else null
    }
}

fun findRobot(mx: List<String>): Pair<Point, Direction> {
    return mx.indices.asSequence().flatMap { y ->
        mx[y].indices.asSequence().mapNotNull { x ->
            val ch = mx[y][x]
            if (ch in "^v<>") {
                val dir = when (ch) {
                    '^' -> Direction(-1, 0)
                    'v' -> Direction(1, 0)
                    '<' -> Direction(0, -1)
                    '>' -> Direction(0, 1)
                    else -> error("Unexpected")
                }
                Point(y, x) to dir
            } else null
        }
    }.first()
}

fun path(input: String): String {
    val mx = screenShot(input)

    val (pos, dir) = findRobot(mx)

    fun look(pos: Point) =
        mx.getOrNull(pos.y)?.getOrNull(pos.x) ?: '.'

    var path = ""
    var finished = false
    var currPos = pos
    var currDir = dir

    while (!finished) {
        finished = true
        val nextPos = Point(currPos.y + currDir.dy, currPos.x + currDir.dx)
        if (look(nextPos) == '#') {
            path += "F"
            currPos = nextPos
            finished = false
        } else {
            // Check for turns only if moving forward is not possible
            listOf(
                Direction(-currDir.dx, currDir.dy) to "L",
                Direction(currDir.dx, -currDir.dy) to "R"
            ).forEach { (nextDir, step) ->
                val nextPosTurn = Point(currPos.y + nextDir.dy, currPos.x + nextDir.dx)
                if (look(nextPosTurn) == '#') {
                    path += step
                    currDir = nextDir
                    finished = false
                    return@forEach
                }
            }
        }
    }

    return path
}

fun evaluatorOne(input: String): Int {
    val mx = screenShot(input)

    val height = mx.size
    val width = mx[0].length

    val cross = listOf(".#.", "###", ".#.")

    fun crossing(y: Int, x: Int): Boolean {
        return dir.all { (dy, dx) -> cross[1 + dy][1 + dx] == mx[y + dy][x + dx] }
    }

    var sum = 0

    for (y in 1 .. (height - 2)) {
        for (x in 1 .. (width - 2)) {
            if (crossing(y, x)) {
                sum += y*x
            }
        }
    }

    return sum
}

fun evaluatorTwo(input: String): Long {
    val program = generatePrograms(path(input)).first()
    val icm = IntCodeMachine(input)
    icm.memory[0] = 2
    return icm.run(program).last()
}

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day17.txt")
        .onSuccess {
            val line = it.first()
            println("Part One: ${evaluatorOne(line)}")
            println("Part Two: ${evaluatorTwo(line)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}