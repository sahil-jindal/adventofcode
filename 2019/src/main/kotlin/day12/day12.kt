package day12

import java.io.File
import kotlin.math.abs
import kotlin.math.sign

data class Point(var x: Int, var y: Int, var z: Int) {
    fun absValue() = abs(x) + abs(y) + abs(z)
}

data class Planet(val pos: Point, val vel: Point)

fun simulate(input: List<String>): Sequence<List<Planet>> = sequence {
    val planets = input.map { line ->
        val (x, y, z) = Regex("-?\\d+").findAll(line).map { it.value.toInt() }.toList()
        val pos = Point(x, y, z)
        val vel = Point(0, 0, 0)
        Planet(pos, vel)
    }

    while (true) {
        for (planetA in planets) {
            for (planetB in planets) {
                planetA.vel.x += (planetB.pos.x - planetA.pos.x).sign
                planetA.vel.y += (planetB.pos.y - planetA.pos.y).sign
                planetA.vel.z += (planetB.pos.z - planetA.pos.z).sign
            }
        }

        for (planet in planets) {
            planet.pos.x += planet.vel.x
            planet.pos.y += planet.vel.y
            planet.pos.z += planet.vel.z
        }

        yield(planets)
    }
}

fun evaluatorOne(input: List<String>): Int {
    return simulate(input).elementAt(999).sumOf { planet ->
        val pot = planet.pos.absValue()
        val kin = planet.vel.absValue()
        pot * kin
    }
}

fun lcm(a: Long, b: Long): Long = a * (b / gcd(a, b))
fun gcd(a: Long, b: Long): Long = if (b == 0L) a else gcd(b, a % b)

fun evaluatorTwo(input: List<String>): Long {
    val states= mutableSetOf<List<Int>>()

    for (planets in simulate(input)) {
        val state = planets.map { it.pos.x } + planets.map { it.vel.x }
        if (!states.add(state)) break
    }

    val statesByX = states.size.toLong()
    states.clear()

    for (planets in simulate(input)) {
        val state = planets.map { it.pos.y } + planets.map { it.vel.y }
        if (!states.add(state)) break
    }

    val statesByY = states.size.toLong()
    states.clear()

    for (planets in simulate(input)) {
        val state = planets.map { it.pos.z } + planets.map { it.vel.z }
        if (!states.add(state)) break
    }

    val statesByZ = states.size.toLong()

    return lcm(statesByX, lcm(statesByY, statesByZ))
}

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day12.txt")
        .onSuccess {
            println("Part One: ${evaluatorOne(it)}")
            println("Part Two: ${evaluatorTwo(it)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}