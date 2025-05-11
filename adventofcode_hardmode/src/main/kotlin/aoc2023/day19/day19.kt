package aoc2023.day19

import java.io.File
import kotlinx.collections.immutable.PersistentList
import kotlinx.collections.immutable.toPersistentList
import java.util.LinkedList

data class Range(val begin: Int, val end: Int)
data class Cond(val dim: Int, val op: Char, val num: Int, val state: String)

typealias Rules = Map<String, String>
typealias Cube = PersistentList<Range>

fun parseRules(input: List<String>): Rules {
    return input.mapNotNull { line ->
        val parts = line.split("{", "}")
        if (parts.size >= 2) parts[0] to parts[1] else null
    }.toMap()
}

fun parseUnitCube(input: List<String>) = input.map { line ->
    val numbers = Regex("\\d+").findAll(line).map { it.value.toInt() }
    numbers.map { n -> Range(n, n) }.toPersistentList()
}

fun parseInput(input: List<String>): Pair<Rules, List<Cube>> {
    val idx = input.indexOf("")
    val rules = parseRules(input.take(idx))
    val cubes = parseUnitCube(input.drop(idx + 1))
    return rules to cubes
}

fun tryParseCond(st: String): Cond? {
    val parts = st.split('<', '>', ':')
    if (parts.size != 3) return null

    val (field, numStr, state) = parts
    val op = st[1] // assumes operator is always at index 1

    val num = numStr.toIntOrNull() ?: return null

    return when (field) {
        "x" -> Cond(0, op, num, state)
        "m" -> Cond(1, op, num, state)
        "a" -> Cond(2, op, num, state)
        "s" -> Cond(3, op, num, state)
        else -> null
    }
}

fun getVolume(cube: Cube) = cube.fold(1L) { acc, r ->
    acc * (r.end - r.begin + 1).toLong()
}

fun cutCube(cube: Cube, dim: Int, num: Int): Pair<Cube, Cube> {
    val r = cube[dim]
    return Pair(
        cube.set(dim, r.copy(end = num.coerceAtMost(r.end))),
        cube.set(dim, r.copy(begin = r.begin.coerceAtLeast(num + 1)))
    )
}

fun acceptedVolume(rules: Rules, cube: Cube): Long {
    val q = LinkedList(listOf(cube to "in"))
    var res = 0L

    while (q.isNotEmpty()) {
        var (cube, state) = q.removeFirst()

        if (cube.any { coord -> coord.end < coord.begin }) {
            continue
        } else if (state == "R") {
            continue
        } else if (state == "A") {
            res += getVolume(cube)
        } else {
            for (stm in rules[state]!!.split(",")) {
                val cond = tryParseCond(stm)
                if (cond == null) {
                    q.add(cube to stm)
                } else if (cond.op == '<') {
                    val (cube1, cube2) = cutCube(cube, cond.dim, cond.num - 1)
                    q.add(cube1 to cond.state)
                    cube = cube2
                } else if (cond.op == '>') {
                    val (cube1, cube2) = cutCube(cube, cond.dim, cond.num)
                    cube = cube1
                    q.add(cube2 to cond.state)
                }
            }
        }
    }

    return res
}

fun evaluatorOne(input: Pair<Rules, List<Cube>>): Int {
    val (rules, cubes) = input
    return cubes.filter { cube -> acceptedVolume(rules, cube) == 1L }.flatten().sumOf { it.begin }
}

fun evaluatorTwo(input: Pair<Rules, List<Cube>>): Long {
    val cube = List(4) { Range(1, 4000) }.toPersistentList()
    return acceptedVolume(input.first, cube)
}

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/aoc2023/day19.txt")
        .onSuccess {
            val input = parseInput(it)
            println("Part One: ${evaluatorOne(input)}")
            println("Part Two: ${evaluatorTwo(input)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}