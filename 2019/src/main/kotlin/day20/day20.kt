package day20

import java.io.File
import java.util.LinkedList
import java.util.Queue

data class Pos2(val y: Int, val x: Int)
data class Pos3(val y: Int, val x: Int, val level: Int)
data class PosD(val y: Int, val x: Int, val dlevel: Int)

fun explore(mx: List<CharArray>): Triple<Map<Pos2, PosD>, Pos3, Pos3> {
    val portals = mutableMapOf<Pos2, PosD>()
    val temp = mutableMapOf<String, Pos2>()

    val height = mx.size
    val width = mx[0].size

    for (y in 0 until height - 1) {
        for (x in 0 until width - 1) {
            for ((dy, dx) in listOf(Pair(0, 1), Pair(1, 0))) {
                val st = "${mx[y][x]}${mx[y + dy][x + dx]}"
                if (st.all { it.isLetter() }) {
                    val portal = if (y - dy >= 0 && x - dx >= 0 && mx[y - dy][x - dx] == '.') {
                        Pos2(y - dy, x - dx)
                    } else {
                        Pos2(y + 2*dy, x + 2*dx)
                    }

                    if (temp.containsKey(st)) {
                        val dlevel = if (portal.x == 2 || portal.x == width - 3 || portal.y == 2 || portal.y == height - 3) -1 else 1
                        portals[portal] = PosD(temp[st]!!.y,  temp[st]!!.x, dlevel)
                        portals[temp[st]!!] = PosD(portal.y, portal.x, -dlevel)
                    } else {
                        temp[st] = portal
                    }

                    mx[y][x] = ' '
                    mx[y + dy][x + dx] = ' '
                }
            }
        }
    }

    return Triple(portals.toMap(), Pos3(temp["AA"]!!.y, temp["AA"]!!.x, 0), Pos3(temp["ZZ"]!!.y, temp["ZZ"]!!.x, 0))
}

fun solve(input: List<String>, partTwo: Boolean): Int {
    val maxWidth = input.maxOf { it.length }
    val mx = input.map { it.padEnd(maxWidth, ' ').toCharArray() }

    val (portals, start, end) = explore(mx)

    fun neighbours(pos: Pos3): Sequence<Pos3> = sequence {
        for ((dy, dx) in listOf(Pair(0, -1), Pair(0, 1), Pair(-1, 0), Pair(1, 0))) {
            yield(Pos3(pos.y + dy, pos.x + dx, pos.level))
        }

        if (portals.containsKey(Pos2(pos.y, pos.x))) {
            var (yT, xT, dlevel) = portals[Pos2(pos.y, pos.x)]!!

            if (!partTwo) {
                dlevel = 0
            }

            if (pos.level + dlevel >= 0) {
                yield(Pos3(yT, xT, pos.level + dlevel))
            }
        }
    }

    val q: Queue<Pair<Pos3, Int>> = LinkedList()
    q.add(Pair(start, 0))

    val seen = mutableSetOf(start)

    while (q.isNotEmpty()) {
        val (pos, dist) = q.remove()

        if (pos == end) {
            return dist
        }

        for (posT in neighbours(pos)) {
            if (!seen.contains(posT)) {
                val disT = dist + 1
                if (mx[posT.y][posT.x] == '.') {
                    seen.add(posT)
                    q.add(Pair(posT, disT))
                }
            }
        }
    }

    throw Exception()
}

fun evaluatorOne(input: List<String>) = solve(input, false)
fun evaluatorTwo(input: List<String>) = solve(input, true)

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day20.txt")
        .onSuccess {
            println("Part One: ${evaluatorOne(it)}")
            println("Part Two: ${evaluatorTwo(it)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}