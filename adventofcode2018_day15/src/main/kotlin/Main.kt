package org.example

import java.io.File

data class Unit(
    val type: Char,
    var x: Int,
    var y: Int,
    var hp: Int = 200,
    val attackPower: Int = 3
) {
    override fun toString(): String {
        return "Unit(type=$type, x=$x, y=$y, hp=$hp)"
    }
}

typealias Grid = List<List<Boolean>>

enum class Direction {
    UP, LEFT, RIGHT, DOWN
}

fun main() {
    val input = File("src/main/resources/input.txt").readLines()
    val (grid, initialUnits) = parseInput(input)

    // Part One
    val (roundsA, totalHpA) = simulateBattle(grid, initialUnits.map { it.copy() })
    println("Part One: ${roundsA * totalHpA}")

    // Part Two
    var elfAttackPower = 4
    while (true) {
        val units = initialUnits.map {
            if (it.type == 'E') it.copy(attackPower = elfAttackPower) else it.copy()
        }
        val (roundsB, totalHpB, elvesDied) = simulateBattle(grid, units, true)
        if (!elvesDied) {
            println("Part Two: ${roundsB * totalHpB} (Elf Attack Power: $elfAttackPower)")
            break
        }
        elfAttackPower++
    }
}

fun parseInput(input: List<String>): Pair<Grid, List<Unit>> {
    val grid = mutableListOf<List<Boolean>>()
    val units = mutableListOf<Unit>()

    for (y in input.indices) {
        val line = input[y]
        val row = mutableListOf<Boolean>()
        for (x in line.indices) {
            when (line[x]) {
                '#' -> row.add(true)
                '.' -> row.add(false)
                'E', 'G' -> {
                    row.add(false)
                    units.add(Unit(line[x], x, y))
                }
                else -> throw IllegalArgumentException("Invalid character in input: ${line[x]}")
            }
        }
        grid.add(row)
    }

    return Pair(grid, units)
}

fun simulateBattle(
    grid: Grid,
    initialUnits: List<Unit>,
    checkElfDeaths: Boolean = false
): Triple<Int, Int, Boolean> {
    val units = initialUnits.toMutableList()
    var rounds = 0
    var combatEnded: Boolean
    var elvesDied = false

    while (true) {
        units.sortWith(compareBy<Unit> { it.y }.thenBy { it.x })
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

// Remaining functions (moveUnit, bfs, attack) remain unchanged as in the previous solution

fun moveUnit(unit: Unit, units: List<Unit>, grid: Grid) {
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

fun bfs(
    startX: Int,
    startY: Int,
    occupiedPositions: Set<Pair<Int, Int>>,
    grid: Grid
): Pair<Map<Pair<Int, Int>, Int>, Map<Pair<Int, Int>, Direction>> {
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

fun attack(unit: Unit, units: List<Unit>): Boolean {
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