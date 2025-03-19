package day24

import java.io.File

data class Point(val y: Int, val x: Int)
data class Position(val ilevel: Int, val y: Int, val x: Int)

fun parseInput(input: List<String>): List<Int> {
    val biodiversity = input.flatMap { it.toList() }
        .mapIndexedNotNull { index, c -> if (c == '#') 1 shl index else null }.sum()

    return listOf(biodiversity)
}

fun positions(): Sequence<Point> = sequence {
    for (y in 0 until 5) {
        for (x in 0 until 5) {
            yield(Point(y, x))
        }
    }
}

fun hasBug(biodiversity: Int, y: Int, x: Int): Boolean {
    return ((biodiversity shr (y*5 + x)) and 1) == 1
}

fun setBug(biodiversity: Int, y: Int, x: Int): Int {
    return biodiversity or (1 shl (y*5 + x))
}

fun step(oldLevelsT: List<Int>, neighbours: (Position) -> Sequence<Position>): List<Int> {
    val oldLevels = oldLevelsT.toMutableList()
    oldLevels.add(0, 0)
    oldLevels.add(0)

    val newLevels = mutableListOf<Int>()

    for (ilevel in oldLevels.indices) {
        var newLevel = 0

        for ((y, x) in positions()) {
            var bugCount = 0

            for ((ilevelT, yT, xT) in neighbours(Position(ilevel, y, x))) {
                if (ilevelT in oldLevels.indices) {
                    bugCount += if (hasBug(oldLevels[ilevelT], yT, xT)) 1 else 0
                }
            }

            if (!hasBug(oldLevels[ilevel], y, x)) {
                if (bugCount == 1 || bugCount == 2) {
                    newLevel = setBug(newLevel, y, x)
                }
            } else {
                if (bugCount == 1) {
                    newLevel = setBug(newLevel, y, x)
                }
            }
        }

        newLevels.add(newLevel)
    }

    return newLevels.toList()
}

fun flatNeighbours(pos: Position): Sequence<Position> = sequence {
    for ((dy, dx) in listOf(0 to 1, 0 to -1, -1 to 0, 1 to 0)) {
        val (yT, xT) = Pair(pos.y + dy, pos.x + dx)
        if (xT in 0..4 && yT in 0..4) {
            yield(Position(pos.ilevel, yT, xT))
        }
    }
}

fun recursiveNeighbours(pos: Position): Sequence<Position> = sequence {
    val (ilevel, y, x) = pos

    for ((dy, dx) in listOf(0 to 1, 0 to -1, -1 to 0, 1 to 0)) {
        var posMin = Point(y + dy, x + dx)
        var posMax = Point(y + dy, x + dx)
        var ilevelT = ilevel

        if (posMin.y == -1) {
            ilevelT = ilevel - 1
            posMin = Point(1, 2)
            posMax = Point(1, 2)
        } else if (posMin.y == 5) {
            ilevelT = ilevel - 1
            posMin = Point(3, 2)
            posMax = Point(3, 2)
        } else if (posMin.x == -1) {
            ilevelT = ilevel - 1
            posMin = Point(2, 1)
            posMax = Point(2, 1)
        } else if (posMin.x == 5) {
            ilevelT = ilevel - 1
            posMin = Point(2, 3)
            posMax = Point(2, 3)
        } else if (posMin == Point(2, 2)) {
            ilevelT = ilevel + 1
            if (dx == 0) {
                posMin = Point(if (dy == 1) 0 else 4, 0)
                posMax = Point(if (dy == 1) 0 else 4, 4)
            } else if (dy == 0) {
                posMin = Point(0, if (dx == 1) 0 else 4)
                posMax = Point(4, if (dx == 1) 0 else 4)
            }
        }

        for (yT in posMin.y..posMax.y) {
            for (xT in posMin.x..posMax.x) {
                yield(Position(ilevelT, yT, xT))
            }
        }
    }
}

fun evaluatorOne(input: List<String>): Int {
    var levels = parseInput(input)
    val seen = mutableSetOf<Int>()
    var biodiversity = levels[0]

    while (!seen.contains(biodiversity)) {
        seen.add(biodiversity)
        levels = step(levels, ::flatNeighbours)
        biodiversity = levels[levels.size shr 1]
    }

    return biodiversity
}

fun evaluatorTwo(input: List<String>): Int {
    var levels = parseInput(input)

    repeat(200) { levels = step(levels, ::recursiveNeighbours) }

    var sum = 0

    for (level in levels) {
        for (pos in positions()) {
            if (pos != Point(2, 2) && hasBug(level, pos.y, pos.x)) {
                sum += 1
            }
        }
    }

    return sum
}

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day24.txt")
        .onSuccess {
            println("Part One: ${evaluatorOne(it)}")
            println("Part Two: ${evaluatorTwo(it)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}