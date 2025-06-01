package aoc2018.day15

import java.io.File

enum class Direction { UP, LEFT, RIGHT, DOWN }

data class Point(val y: Int, val x: Int) : Comparable<Point> {
    override fun compareTo(other: Point): Int {
        return compareValuesBy(this, other, Point::y, Point::x)
    }
}

data class CombatUnit(val type: Char, var pos: Point, val attackPower: Int = 3, var hp: Int = 200)

typealias Grid = Map<Point, Boolean>

fun parseInput(input: List<String>): Pair<Grid, List<CombatUnit>> {
    val grid = mutableMapOf<Point, Boolean>()
    val units = mutableListOf<CombatUnit>()

    for ((y, line) in input.withIndex()) {
        for ((x, ch) in line.withIndex()) {
            val pos = Point(y, x)
            when (ch) {
                '#' -> grid.put(pos, true)
                '.' -> grid.put(pos, false)
                'E', 'G' -> {
                    grid.put(pos, false)
                    units.add(CombatUnit(ch, pos))
                }
                else -> throw IllegalArgumentException("Invalid character in input: $ch")
            }
        }
    }

    return grid.toMap() to units.toList()
}

fun bfs(start: Point, occupiedPositions: Set<Point>, grid: Grid): Pair<Map<Point, Int>, Map<Point, Direction>> {
    val queue = ArrayDeque(listOf(start))
    val distanceMap = mutableMapOf(start to 0)
    val firstStepDirMap = mutableMapOf<Point, Direction>()

    while (queue.isNotEmpty()) {
        val currentPos = queue.removeFirst()
        val currentDistance = distanceMap[currentPos]!!

        val directions = listOf(
            Direction.UP to Point(currentPos.y - 1, currentPos.x),
            Direction.LEFT to Point(currentPos.y, currentPos.x - 1),
            Direction.RIGHT to Point(currentPos.y, currentPos.x + 1),
            Direction.DOWN to Point(currentPos.y + 1, currentPos.x),
        )

        for ((move, newPos) in directions) {
            if (grid.contains(newPos) && !grid[newPos]!! && !occupiedPositions.contains(newPos)) {
                if (!distanceMap.containsKey(newPos)) {
                    distanceMap[newPos] = currentDistance + 1
                    val firstStepDir = if (currentPos == start) move else firstStepDirMap[currentPos]!!
                    firstStepDirMap[newPos] = firstStepDir
                    queue.add(newPos)
                } else if (distanceMap[newPos] == currentDistance + 1) {
                    val existingFirstStep = firstStepDirMap[newPos]!!
                    val newFirstStep = if (currentPos == start) move else firstStepDirMap[currentPos]!!
                    if (newFirstStep.ordinal < existingFirstStep.ordinal) {
                        firstStepDirMap[newPos] = newFirstStep
                    }
                }
            }
        }
    }

    return distanceMap.toMap() to firstStepDirMap.toMap()
}

fun getNeighbours(pos: Point) = listOf(
    pos.copy(x = pos.x - 1),
    pos.copy(x = pos.x + 1),
    pos.copy(y = pos.y - 1),
    pos.copy(y = pos.y + 1)
)

fun moveUnit(unit: CombatUnit, units: List<CombatUnit>, grid: Grid) {
    val occupiedPositions = units.filter { it.hp > 0 && it != unit }.map { it.pos }.toSet()
    val enemies = units.filter { it.type != unit.type && it.hp > 0 }

    if (enemies.isEmpty()) return

    val inRangeSquares = enemies.flatMap { getNeighbours(it.pos) }
        .filter { grid.contains(it) && !grid[it]!! && !occupiedPositions.contains(it) }
        .toSet()

    if (inRangeSquares.isEmpty()) return

    val (distanceMap, firstStepDirMap) = bfs(unit.pos, occupiedPositions, grid)

    val reachableSquares = inRangeSquares.filter { distanceMap.containsKey(it) }

    if (reachableSquares.isEmpty()) return

    val targetSquare = reachableSquares.minWithOrNull(compareBy<Point> { distanceMap[it]!! }.thenBy { it }) ?: return

    val movement = firstStepDirMap[targetSquare] ?: return

    unit.pos = when (movement) {
        Direction.UP -> unit.pos.copy(y = unit.pos.y - 1)
        Direction.LEFT -> unit.pos.copy(x = unit.pos.x - 1)
        Direction.RIGHT -> unit.pos.copy(x = unit.pos.x + 1)
        Direction.DOWN -> unit.pos.copy(y = unit.pos.y + 1)
    }
}

fun attack(unit: CombatUnit, units: List<CombatUnit>): Boolean {
    val adjacentEnemies = getNeighbours(unit.pos).mapNotNull { newPos ->
        units.find { it.pos == newPos && it.type != unit.type && it.hp > 0 }
    }

    if (adjacentEnemies.isEmpty()) return false

    val targetEnemy = adjacentEnemies.minWithOrNull(compareBy<CombatUnit> { it.hp }.thenBy { it.pos }) ?: return false

    targetEnemy.hp -= unit.attackPower

    return targetEnemy.type == 'E' && targetEnemy.hp <= 0
}

fun simulateBattle(grid: Grid, initialUnits: List<CombatUnit>, checkElfDeaths: Boolean): Triple<Int, Int, Boolean> {
    val units = initialUnits.toMutableList()
    var rounds = 0
    var combatEnded = false
    var elvesDied = false

    while (!combatEnded) {
        units.sortWith(compareBy { it.pos })

        val unitsToProcess = units.toList()

        for (unit in unitsToProcess) {
            if (unit.hp <= 0) continue

            val enemies = units.filter { it.type != unit.type && it.hp > 0 }

            if (enemies.isEmpty()) {
                combatEnded = true
                break
            }

            moveUnit(unit, units, grid)

            val elfKilled = attack(unit, units)

            if (checkElfDeaths && elfKilled) {
                elvesDied = true
            }
        }

        units.removeAll { it.hp <= 0 }
        rounds++
    }

    val totalHp = units.sumOf { it.hp }

    return Triple(rounds - 1, totalHp, elvesDied)
}

fun evaluatorOne(input: List<String>): Int {
    val (grid, initialUnits) = parseInput(input)
    val (rounds, totalHp) = simulateBattle(grid, initialUnits, false)
    return rounds * totalHp
}

fun evaluatorTwo(input: List<String>): Int {
    val (grid, initialUnits) = parseInput(input)

    var elfAttackPower = 4

    while (true) {
        val units = initialUnits.map { if (it.type == 'E') it.copy(attackPower = elfAttackPower) else it.copy() }
        val (rounds, totalHp, elvesDied) = simulateBattle(grid, units, true)
        if (!elvesDied) return rounds * totalHp
        elfAttackPower++
    }
}

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/aoc2018/day15.txt")
        .onSuccess {
            println("Part One: ${evaluatorOne(it)}")
            println("Part Two: ${evaluatorTwo(it)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}