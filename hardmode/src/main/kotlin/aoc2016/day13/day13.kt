package aoc2016.day13

import java.util.LinkedList

data class Point(val y: Int, val x: Int)

fun getNeighbours(pos: Point) = listOf(
    pos.copy(x = pos.x - 1),
    pos.copy(x = pos.x + 1),
    pos.copy(y = pos.y - 1),
    pos.copy(y = pos.y + 1)
)

fun steps(input: Int) = sequence {
    val start = Point(1, 1)

    val queue = LinkedList(listOf(0 to start))
    val seen = mutableSetOf(start)

    while (queue.isNotEmpty()) {
        val (stepCount, pos) = queue.remove()
        yield(stepCount to pos)

        for (newPos in getNeighbours(pos)) {
            if (newPos.y >= 0 && newPos.x >= 0 && newPos !in seen) {
                val (y, x) = newPos
                val w = x*x + 3*x + 2*x*y + y + y*y + input
                if (w.countOneBits() % 2 == 0) {
                    seen.add(newPos)
                    queue.add(Pair(stepCount + 1, newPos))
                }
            }
        }
    }
}

fun evaluatorOne(input: Int): Int {
    return steps(input).first { it.second == Point(39, 31) }.first
}

fun evaluatorTwo(input: Int): Int {
    return steps(input).takeWhile { it.first <= 50 }.count()
}

fun main() {
    val inputLine = 1350
    println("Part One: ${evaluatorOne(inputLine)}")
    println("Part Two: ${evaluatorTwo(inputLine)}")
}