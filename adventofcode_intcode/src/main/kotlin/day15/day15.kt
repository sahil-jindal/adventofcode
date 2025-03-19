package day15

import intcode.ImmutableIntCodeMachine
import java.io.File
import java.util.LinkedList
import java.util.Queue

enum class Tile { Wall, Empty, O2 }

data class Point(val x: Int, val y: Int)
data class Direction(val dx: Int, val dy: Int)
data class GroupOne(val iicm: ImmutableIntCodeMachine, val path: List<Int>, val tile: Tile)
data class GroupTwo(val iicm: ImmutableIntCodeMachine, val path: List<Int>, val p: Point)

fun bfs(startIicm: ImmutableIntCodeMachine): Sequence<GroupOne> = sequence {
    val dirs = listOf(Direction(0, -1), Direction(0, 1), Direction(-1, 0), Direction(1, 0))

    val startPoint = Point(0, 0)

    val q: Queue<GroupTwo> = LinkedList()
    q.add(GroupTwo(startIicm, emptyList(), startPoint))

    val seen = mutableSetOf(startPoint)

    while (q.isNotEmpty()) {
        val (iicm, path, p) = q.remove()

        for (i in dirs.indices) {
            val next = Point(p.x + dirs[i].dx, p.y + dirs[i].dy)

            if (!seen.contains(next)) {
                seen.add(next)

                val nextPath = path.plus(i + 1)
                val (nextIicm, output) = iicm.run((i + 1).toLong())

                val tile = Tile.entries[output.single().toInt()]

                if (tile != Tile.Wall) {
                    yield(GroupOne(nextIicm, nextPath, tile))
                    q.add(GroupTwo(nextIicm, nextPath, next))
                }
            }
        }
    }
}

fun evaluatorOne(input: String): Int {
    val iicm = ImmutableIntCodeMachine(input)
    return bfs(iicm).first { it.tile == Tile.O2 }.path.count()
}

fun evaluatorTwo(input: String): Int {
    val iicm = bfs(ImmutableIntCodeMachine(input)).first { it.tile == Tile.O2 }.iicm
    return bfs(iicm).last().path.count()
}

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day15.txt")
        .onSuccess {
            val line = it.first()
            println("Part One: ${evaluatorOne(line)}")
            println("Part Two: ${evaluatorTwo(line)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}