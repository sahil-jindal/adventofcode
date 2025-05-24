package aoc2024.day16

import java.io.File
import java.util.PriorityQueue

data class Direction(val dy: Int, val dx: Int) {
    operator fun unaryMinus() = Direction(-dy, -dx)
}

data class Point(val y: Int, val x: Int) {
    operator fun plus(dir: Direction) = Point(y + dir.dy, x + dir.dx)
    operator fun minus(dir: Direction) = Point(y - dir.dy, x - dir.dx)
}

data class State(val pos: Point, val dir: Direction)

typealias Grid = Map<Point, Char>

val North = Direction(-1, 0)
val South = Direction(1, 0)
val West = Direction(0, -1)
val East = Direction(0, 1)

fun getMap(input: List<String>): Grid {
    val res = mutableMapOf<Point, Char>()

    for ((y, line) in input.withIndex()) {
        for ((x, ch) in line.withIndex()) {
            res.put(Point(y, x), ch)
        }
    }

    return res.toMap()
}

// returns the possible next or previous states and the associated costs for a given state.
// in forward mode we scan the possible states from the start state towards the goal.
// in backward mode we are working backwards from the goal to the start.
fun steps(map: Grid, state: State, forward: Boolean) = sequence {
    for (dir in listOf(North, East, West, South)) {
        if (dir == state.dir) {
            val pos = if (forward) state.pos + dir else state.pos - dir
            if (map.getOrDefault(pos, ' ') != '#') {
                yield(State(pos, dir) to 1)
            }
        } else if (dir != -state.dir) {
            yield(State(state.pos, dir) to 1000)
        }
    }
}

fun dijkstra(map: Grid, goal: Point): Map<State, Int> {
    // Dijkstra algorithm; works backwards from the goal, returns the
    // distances to _all_ tiles and directions.
    val dist = mutableMapOf<State, Int>()
    val pq = PriorityQueue(compareBy<Pair<State, Int>> { it.second })

    for (dir in listOf(North, East, West, South)) {
        val state = State(goal, dir)
        dist[state] = 0
        pq.add(state to 0)
    }

    while (pq.isNotEmpty()) {
        val (cur, curCost) = pq.poll()

        for ((next, stepCost) in steps(map, cur, forward = false)) {
            val nextCost = curCost + stepCost
            if (nextCost < dist.getOrDefault(next, Int.MAX_VALUE)) {
                pq.removeIf { it.first == next }
                dist[next] = nextCost
                pq.add(next to nextCost) // Allow duplicates
            }
        }
    }

    return dist.toMap()
}

fun goal(map: Grid) = map.keys.single { map[it] == 'E' }
fun start(map: Grid) = State(map.keys.single { map[it] == 'S' }, East)

fun solver(input: List<String>) {
    val map = getMap(input)

    val dist = dijkstra(map, goal(map))
    val start = start(map)
    println("Part One: ${dist[start]}")

    val pq = PriorityQueue(compareBy<Pair<State, Int>> { it.second })
    val bestSpots = mutableSetOf(start)

    pq.add(start to dist[start]!!)

    while (pq.isNotEmpty()) {
        val (state, remainingScore) = pq.poll()

        for ((next, score) in steps(map, state, forward = true)) {
            val nextRemainingScore = remainingScore - score

            if (!bestSpots.contains(next) && dist[next] == nextRemainingScore) {
                bestSpots.add(next)
                pq.add(next to nextRemainingScore)
            }
        }
    }

    println("Part Two: ${bestSpots.distinctBy { it.pos }.count()}")
}

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/aoc2024/day16.txt")
        .onSuccess { solver(it) }
        .onFailure { println("Error reading file: ${it.message}") }
}