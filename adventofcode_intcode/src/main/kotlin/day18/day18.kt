package day18

import java.io.File
import java.util.*

data class Point(val y: Int, val x: Int)
data class Direction(val dy: Int, val dx: Int)
data class Edge(val key: Char, val dist: Int, val req: Int)

class Maze(private val maze: List<String>): List<String> by maze {
    private val height = maze.size
    private val width = maze[0].length

    fun getStartPositions(): List<Point> {
        val starts = mutableListOf<Point>()

        for (y in maze.indices) {
            for (x in maze[y].indices) {
                if (maze[y][x] == '@') starts.add(Point(y, x))
            }
        }

        return starts
    }

    fun getKeyPositions(): Map<Char, Point> {
        val keys = mutableMapOf<Char, Point>()

        for (y in maze.indices) {
            for (x in maze[y].indices) {
                val c = maze[y][x]
                if (c.isLowerCase()) keys[c] = Point(y, x)
            }
        }

        return keys
    }

    fun getAdjacent(pos: Point): List<Point> {
        return listOf(
            Direction(-1, 0), Direction(1, 0), Direction(0, -1), Direction(0, 1)
        ).map { Point(pos.y + it.dy, pos.x + it.dx) }
            .filter { it.y in 0 until height && it.x in 0 until width }
            .filter { maze[it.y][it.x] != '#' }
    }
}

fun solveSingle(start: Point, maze: Maze, allKeys: Set<Char>): Int {
    val keyMask = mutableMapOf<Char, Int>()

    allKeys.forEachIndexed { idx, c -> keyMask[c] = 1 shl idx }

    val totalKeys = allKeys.size
    val targetMask = (1 shl totalKeys) - 1

    val visited = mutableSetOf<Pair<Point, Int>>()

    val queue = PriorityQueue<Pair<Int, Pair<Point, Int>>>(compareBy { it.first })

    queue.add(0 to (start to 0))

    while (queue.isNotEmpty()) {
        val (dist, state) = queue.poll()
        val (pos, mask) = state

        if (mask == targetMask) return dist

        if (visited.contains(state)) continue
        visited.add(state)

        for (nextPos in maze.getAdjacent(pos)) {
            val c = maze[nextPos.y][nextPos.x]
            var newMask = mask

            if (c.isUpperCase()) {
                val requiredKey = c.lowercaseChar()
                if (keyMask[requiredKey]?.let { (mask and it) == 0 } == true) continue
            }

            if (c.isLowerCase()) {
                newMask = newMask or (keyMask[c] ?: 0)
            }

            if (!visited.contains(nextPos to newMask)) {
                queue.add(dist + 1 to (nextPos to newMask))
            }
        }
    }

    return Int.MAX_VALUE
}

fun solveMultiOptimized(starts: List<Point>, maze: Maze, allKeys: Set<Char>): Int {
    // Precompute a bit for each key.
    val keyMask = mutableMapOf<Char, Int>()

    allKeys.forEachIndexed { idx, c -> keyMask[c] = 1 shl idx }

    val totalKeys = allKeys.size
    val targetMask = (1 shl totalKeys) - 1

    // BFS from a given point: computes reachable keys along with the distance and the keys required

    fun bfsFrom(start: Point): Map<Char, Pair<Int, Int>> {
        val queue: Queue<Triple<Point, Int, Int>> = LinkedList()
        queue.add(Triple(start, 0, 0))

        val seen = mutableSetOf<Point>()

        val result = mutableMapOf<Char, Pair<Int, Int>>()

        while (queue.isNotEmpty()) {
            val (p, dist, req) = queue.remove()
            if (p in seen) continue
            seen.add(p)

            val c = maze[p.y][p.x]
            var newReq = req

            if (c.isUpperCase()) {
                // Add the required key bit for a door.
                newReq = req or (keyMask[c.lowercaseChar()] ?: 0)
            }

            // If we find a key (and we aren’t starting on it) record it.
            if (c.isLowerCase() && dist > 0) {
                result[c] = dist to newReq
            }

            // Continue exploring.
            for (n in maze.getAdjacent(p)) {
                if (n !in seen) {
                    queue.add(Triple(n, dist + 1, newReq))
                }
            }
        }

        return result
    }

    // Build the key graph.
    // Each node is identified by a String.
    // For starting positions, we use identifiers "0", "1", "2", "3", etc.
    // For keys, we use their lowercase letter (e.g. "a", "b", etc.)
    val sources = mutableMapOf<String, Point>()

    for ((i, p) in starts.withIndex()) {
        sources[i.toString()] = p
    }
    for ((k, p) in maze.getKeyPositions()) {
        sources[k.toString()] = p
    }

    val graph = mutableMapOf<String, List<Edge>>()

    for ((id, p) in sources) {
        val reachable = bfsFrom(p)
        graph[id] = reachable.map { (key, pair) -> Edge(key, pair.first, pair.second) }
    }

    // DFS with memoization over the state:
    // - positions: the current node (as an identifier) for each robot
    // - collected: bitmask of keys collected so far

    val memo = mutableMapOf<Pair<List<String>, Int>, Int>()

    fun dfs(positions: List<String>, collected: Int): Int {
        if (collected == targetMask) return 0

        val state = positions to collected

        if (state in memo) return memo[state]!!

        var best = Int.MAX_VALUE

        // Try moving each robot.
        for (i in positions.indices) {
            val pos = positions[i]
            for (edge in graph[pos] ?: emptyList()) {
                val keyBit = keyMask[edge.key]!!
                if ((collected and keyBit) != 0) continue           // Key already collected.
                if ((collected and edge.req) != edge.req) continue     // Missing required key(s).
                val newCollected = collected or keyBit
                val newPositions = positions.toMutableList()
                newPositions[i] = edge.key.toString()  // Move robot i to the new key's position.
                val cost = edge.dist + dfs(newPositions, newCollected)
                if (cost < best) best = cost
            }
        }

        memo[state] = best
        return best
    }

    val initialPositions = starts.indices.map { it.toString() }
    return dfs(initialPositions, 0)
}

fun evaluatorOne(input: List<String>): Int {
    val maze = Maze(input)
    val start = maze.getStartPositions().single()
    val allKeys = maze.getKeyPositions().keys
    return solveSingle(start, maze, allKeys)
}

fun evaluatorTwo(input: List<String>): Int {
    val modifiedInput = input.toMutableList()
    val midY = input.size / 2
    val midX = input[0].length / 2

    // Modify the maze to split into 4 robots
    modifiedInput[midY-1] = modifiedInput[midY-1].replaceRange(midX-1..midX+1, "@#@")
    modifiedInput[midY] = modifiedInput[midY].replaceRange(midX-1..midX+1, "###")
    modifiedInput[midY+1] = modifiedInput[midY+1].replaceRange(midX-1..midX+1, "@#@")

    val maze = Maze(modifiedInput)
    val starts = maze.getStartPositions()

    require(starts.size == 4) { "Expected 4 start positions" }

    val allKeys = maze.getKeyPositions().keys
    return solveMultiOptimized(starts, maze, allKeys)
}

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day18.txt")
        .onSuccess {
            println("Part One: ${evaluatorOne(it)}")
            println("Part Two: ${evaluatorTwo(it)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}