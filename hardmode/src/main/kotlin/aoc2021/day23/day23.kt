package aoc2021.day23

import java.io.File
import java.util.*
import kotlin.math.abs

data class PairOne(val maze: Maze, val cost: Int)

fun getXDst(ch: Char): Int {
    if (ch == 'A') return 3
    if (ch == 'B') return 5
    if (ch == 'C') return 7
    if (ch == 'D') return 9

    throw Exception()
}

fun stepCost(actor: Char): Int {
    if (actor == 'A') return 1
    if (actor == 'B') return 10
    if (actor == 'C') return 100
    return 1000
}

fun hallwayToRoom(maze: Maze): PairOne {
    for (x in 1 until 12) {
        val ch = maze.itemAt(Point(1, x))

        if (ch == '.') continue

        val xDst = getXDst(ch)

        if (maze.canMoveToDoor(x, xDst) && maze.canEnterRoom(ch)) {
            var steps = abs(xDst - x)
            var pt = Point(1, xDst)

            while (maze.itemAt(pt.below()) == '.') {
                pt = pt.below()
                steps++
            }

            val l = hallwayToRoom(maze.move(Point(1, x), pt))
            return PairOne(l.maze, l.cost + steps * stepCost(ch))
        }
    }

    return PairOne(maze, 0)
}

fun roomToHallway(maze: Maze): List<PairOne> {
    val result = mutableListOf<PairOne>()

    val hallwayColumns = listOf(1, 2, 4, 6, 8, 10, 11)

    for (roomColumn in listOf(3, 5, 7, 9)) {
        if (maze.finishedColumn(roomColumn)) continue

        var stepsV = 0
        var ptSrc = Point(1, roomColumn)

        while (maze.itemAt(ptSrc) == '.') {
            ptSrc = ptSrc.below()
            stepsV++
        }

        val ch = maze.itemAt(ptSrc)

        if (ch == '#') continue

        for (dj in listOf(-1, 1)) {
            var stepsH = 0
            var ptDst = Point(1, roomColumn)

            while (maze.itemAt(ptDst) == '.') {
                if (hallwayColumns.contains(ptDst.x)) {
                    result.add(PairOne(maze.move(ptSrc, ptDst), (stepsV + stepsH) * stepCost(ch)))
                }

                ptDst = if (dj == -1) ptDst.left() else ptDst.right()
                stepsH++
            }
        }
    }

    return result.toList()
}

fun neighbours(maze: Maze): List<PairOne> {
    val hallwayToRoom = hallwayToRoom(maze)
    return if (hallwayToRoom.cost != 0) listOf(hallwayToRoom) else roomToHallway(maze)
}

fun solve(input: List<String>): Int {
    var maze = Maze.parseInput(input)

    val q = PriorityQueue<PairOne>(compareBy { it.cost })
    val cost = mutableMapOf<Maze, Int>()

    q.add(PairOne(maze, 0))
    cost[maze] = 0

    while (q.isNotEmpty()) {
        maze = q.remove().maze

        if (maze.finished()) {
            return cost[maze]!!
        }

        for (n in neighbours(maze)) {
            if (cost[maze]!! + n.cost < cost.getOrDefault(n.maze, Int.MAX_VALUE)) {
                cost[n.maze] = cost[maze]!! + n.cost
                q.add(PairOne(n.maze, cost[n.maze]!!))
            }
        }
    }

    throw Exception()
}

fun upscale(input: List<String>): List<String> {
    val lines = input.toMutableList()
    lines.add(3, "  #D#C#B#A#")
    lines.add(4, "  #D#B#A#C#")
    return lines.toList()
}

fun evaluatorOne(input: List<String>): Int = solve(input)
fun evaluatorTwo(input: List<String>): Int = solve(upscale(input))

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/aoc2021/day23.txt")
        .onSuccess {
            println("Part One: ${evaluatorOne(it)}")
            println("Part Two: ${evaluatorTwo(it)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}