package aoc2018.day15

import java.io.File

enum class Direction { UP, LEFT, RIGHT, DOWN }

data class CombatUnit(val type: Char, var x: Int, var y: Int, var hp: Int = 200, val attackPower: Int = 3)

typealias Grid = List<List<Boolean>>

fun parseInput(input: List<String>): Pair<Grid, List<CombatUnit>> {
    val grid = mutableListOf<List<Boolean>>()
    val units = mutableListOf<CombatUnit>()

    for (y in input.indices) {
        val line = input[y]
        val row = mutableListOf<Boolean>()
        for (x in line.indices) {
            when (line[x]) {
                '#' -> row.add(true)
                '.' -> row.add(false)
                'E', 'G' -> {
                    row.add(false)
                    units.add(CombatUnit(line[x], x, y))
                }
                else -> throw IllegalArgumentException("Invalid character in input: ${line[x]}")
            }
        }
        grid.add(row)
    }

    return Pair(grid, units)
}

fun bfs(startX: Int, startY: Int, occupiedPositions: Set<Pair<Int, Int>>, grid: Grid): Pair<Map<Pair<Int, Int>, Int>, Map<Pair<Int, Int>, Direction>> {
    val queue = ArrayDeque<Pair<Int, Int>>()
    val distanceMap = mutableMapOf<Pair<Int, Int>, Int>()
    val firstStepDirMap = mutableMapOf<Pair<Int, Int>, Direction>()

    queue.add(Pair(startX, startY))
    distanceMap[Pair(startX, startY)] = 0

    while (queue.isNotEmpty()) {
        val (currentX, currentY) = queue.removeFirst()
        val currentDistance = distanceMap[currentX to currentY] ?: continue

        listOf(
            Direction.UP to (0 to -1),
            Direction.LEFT to (-1 to 0),
            Direction.RIGHT to (1 to 0),
            Direction.DOWN to (0 to 1)
        ).forEach {
            val nextX = currentX + it.second.first
            val nextY = currentY + it.second.second

            if (nextY < 0 || nextY >= grid.size || nextX < 0 || nextX >= grid[nextY].size) return@forEach
            if (grid[nextY][nextX]) return@forEach
            if (occupiedPositions.contains(nextX to nextY)) return@forEach

            val nextPos = nextX to nextY

            if (!distanceMap.containsKey(nextPos)) {
                distanceMap[nextPos] = currentDistance + 1

                val firstStepDir = if (currentX == startX && currentY == startY) {
                    it.first
                } else {
                    firstStepDirMap[currentX to currentY]
                }

                firstStepDirMap[nextPos] = firstStepDir!!
                queue.add(nextPos)
            } else if (distanceMap[nextPos] == currentDistance + 1) {
                val existingFirstStep = firstStepDirMap[nextPos]

                val newFirstStep = if (currentX == startX && currentY == startY) {
                    it.first
                } else {
                    firstStepDirMap[currentX to currentY]
                }

                if (newFirstStep != null && (existingFirstStep == null || newFirstStep.ordinal < existingFirstStep.ordinal)) {
                    firstStepDirMap[nextPos] = newFirstStep
                }
            }
        }
    }

    return distanceMap to firstStepDirMap
}

fun moveUnit(unit: CombatUnit, units: List<CombatUnit>, grid: Grid) {
    val occupiedPositions = units
        .filter { it.hp > 0 && it != unit }
        .map { Pair(it.x, it.y) }
        .toSet()

    val enemies = units.filter { it.type != unit.type && it.hp > 0 }

    if (enemies.isEmpty()) return

    val inRangeSquares = mutableSetOf<Pair<Int, Int>>()

    for (enemy in enemies) {
        listOf(
            Pair(enemy.x, enemy.y - 1),
            Pair(enemy.x - 1, enemy.y),
            Pair(enemy.x + 1, enemy.y),
            Pair(enemy.x, enemy.y + 1)
        ).forEach { (nx, ny) ->
            if (ny < 0 || ny >= grid.size || nx < 0 || nx >= grid[ny].size) return@forEach
            if (grid[ny][nx]) return@forEach
            if (occupiedPositions.contains(Pair(nx, ny))) return@forEach
            inRangeSquares.add(Pair(nx, ny))
        }
    }

    if (inRangeSquares.isEmpty()) return

    val (distanceMap, firstStepDirMap) = bfs(unit.x, unit.y, occupiedPositions, grid)

    val reachableSquares = inRangeSquares.filter { distanceMap.containsKey(it) }

    if (reachableSquares.isEmpty()) return

    val targetSquare = reachableSquares.minWithOrNull(compareBy(
        { distanceMap[it]!! },
        { it.second },
        { it.first }
    )) ?: return

    val direction = firstStepDirMap[targetSquare] ?: return

    when (direction) {
        Direction.UP -> unit.y--
        Direction.LEFT -> unit.x--
        Direction.RIGHT -> unit.x++
        Direction.DOWN -> unit.y++
    }
}

fun attack(unit: CombatUnit, units: List<CombatUnit>): Boolean {
    val adjacentEnemies = listOf(
        unit.x to unit.y - 1,
        unit.x - 1 to unit.y,
        unit.x + 1 to unit.y,
        unit.x to unit.y + 1
    ).mapNotNull { (nx, ny) ->
        units.find { it.x == nx && it.y == ny && it.type != unit.type && it.hp > 0 }
    }

    if (adjacentEnemies.isEmpty()) return false

    val targetEnemy = adjacentEnemies.minWithOrNull(compareBy(
        { it.hp },
        { it.y },
        { it.x }
    )) ?: return false

    targetEnemy.hp -= unit.attackPower
    return targetEnemy.type == 'E' && targetEnemy.hp <= 0
}

fun simulateBattle(grid: Grid, initialUnits: List<CombatUnit>, checkElfDeaths: Boolean = false): Triple<Int, Int, Boolean> {
    val units = initialUnits.toMutableList()
    var rounds = 0
    var combatEnded: Boolean
    var elvesDied = false

    while (true) {
        units.sortWith(compareBy<CombatUnit> { it.y }.thenBy { it.x })
        combatEnded = false

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

        if (combatEnded) {
            break
        }

        units.removeAll { it.hp <= 0 }
        rounds++
    }

    // Filter out dead units before summing HP
    val remainingUnits = units.filter { it.hp > 0 }
    val totalHp = remainingUnits.sumOf { it.hp }

    return Triple(rounds, totalHp, elvesDied)
}

fun evaluatorOne(input: List<String>): Int {
    val (grid, initialUnits) = parseInput(input)
    val (rounds, totalHp) = simulateBattle(grid, initialUnits)
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