package day10

import java.io.File
import kotlin.math.abs
import kotlin.math.atan2

data class Point(val y: Int, val x: Int)
data class Direction(val dy: Int, val dx: Int)

private typealias AsteroidsByDir = MutableMap<Direction, MutableList<Point>>

fun gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

fun asteroids(map: List<String>): List<Point> {
    return map.indices.flatMap { iy ->
        map[iy].indices.mapNotNull { ix ->
            if (map[iy][ix] == '#') Point(iy, ix) else null
        }
    }
}

fun selectStationPosition(input: List<String>): Pair<Point, AsteroidsByDir> {
    var res = Point(0, 0) to mutableMapOf<Direction, MutableList<Point>>()
    val asteroids = asteroids(input)

    for (station in asteroids) {
        val asteroidsByDir: AsteroidsByDir = mutableMapOf()
        for (asteroid in asteroids) {
            if (station == asteroid) continue
            val yDir = asteroid.y - station.y
            val xDir = asteroid.x - station.x
            val gcd = abs(gcd(yDir, xDir))
            val dir = Direction(yDir / gcd, xDir / gcd)
            asteroidsByDir.computeIfAbsent(dir) { mutableListOf() }.add(asteroid)
        }
        if (asteroidsByDir.size > res.second.size) {
            res = station to asteroidsByDir
        }
    }

    return res
}

fun rotate(dirs: Set<Direction>): Sequence<Direction> {
    val ordered = dirs.sortedByDescending { atan2(it.dx.toDouble(), it.dy.toDouble()) }
    return generateSequence(0) { it + 1 }.map { ordered[it % ordered.size] }
}

fun destroy(input: List<String>): Sequence<Point> = sequence {
    val (station, asteroidsByDir) = selectStationPosition(input)

    asteroidsByDir.keys.toList().forEach { dir ->
        asteroidsByDir[dir] = asteroidsByDir[dir]!!
            .sortedBy { abs(it.y - station.y) + abs(it.x - station.x) }
            .toMutableList()
    }

    for (dir in rotate(asteroidsByDir.keys)) {
        asteroidsByDir[dir]?.let { list ->
            val asteroid = list.removeAt(0)
            yield(asteroid)
            if (list.isEmpty()) asteroidsByDir.remove(dir)
        }
    }
}

fun evaluatorOne(input: List<String>): Int = selectStationPosition(input).second.size

fun evaluatorTwo(input: List<String>): Int {
    val asteroid = destroy(input).elementAt(199)
    return asteroid.x * 100 + asteroid.y
}

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day10.txt")
        .onSuccess {
            println("Part One: ${evaluatorOne(it)}")
            println("Part Two: ${evaluatorTwo(it)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}