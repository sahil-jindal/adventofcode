package aoc2018.day24

import java.io.File
import java.util.*
import kotlin.math.max

class Group(
    val immuneSystem: Boolean,
    var units: Long,
    val hp: Int,
    var damage: Int,
    val initiative: Int,
    private val attackType: String,
    var immuneTo: MutableSet<String> = mutableSetOf(),
    var weakTo: MutableSet<String> = mutableSetOf()
) {
    val effectivePower: Long
        get() = units * damage

    fun damageDealtTo(target: Group): Long = when {
        target.immuneSystem == immuneSystem -> 0
        target.immuneTo.contains(attackType) -> 0
        target.weakTo.contains(attackType) -> effectivePower * 2
        else -> effectivePower
    }
}

fun parseInput(lines: List<String>): List<Group> {
    var immuneSystem = false
    val res = mutableListOf<Group>()
    val rx = Regex("""(\d+) units each with (\d+) hit points(.*)with an attack that does (\d+)(.*)damage at initiative (\d+)""")

    for (line in lines) {
        when {
            line == "Immune System:" -> immuneSystem = true
            line == "Infection:" -> immuneSystem = false
            line.isNotEmpty() -> {
                val match = rx.find(line)
                if (match != null) {
                    val g = Group(
                        immuneSystem,
                        match.groupValues[1].toLong(),
                        match.groupValues[2].toInt(),
                        match.groupValues[4].toInt(),
                        match.groupValues[6].toInt(),
                        match.groupValues[5].trim(),
                    )
                    val st = match.groupValues[3].trim()
                    if (st.isNotEmpty()) {
                        val status = st.substring(1, st.length - 1).split(";")
                        for (part in status) {
                            val (key, values) = part.split(" to ")
                            val set = values.split(", ").toMutableSet()
                            when (key.trim()) {
                                "immune" -> g.immuneTo = set
                                "weak" -> g.weakTo = set
                                else -> throw Exception()
                            }
                        }
                    }
                    res.add(g)
                } else {
                    throw Exception()
                }
            }
        }
    }
    
    return res
}

fun fight(input: List<String>, boost: Int): Pair<Boolean, Long> {
    var army = parseInput(input)
    
    for (g in army) {
        if (g.immuneSystem) {
            g.damage += boost
        }
    }
    
    var attack = true
    
    while (attack) {
        attack = false
        val remainingTarget = HashSet(army)
        val targets = mutableMapOf<Group, Group>()
        
        for (g in army.sortedWith(compareByDescending<Group> { it.effectivePower }.thenByDescending { it.initiative })) {
            val maxDamage = remainingTarget.maxOfOrNull { g.damageDealtTo(it) } ?: 0
            if (maxDamage > 0) {
                val possibleTargets = remainingTarget.filter { g.damageDealtTo(it) == maxDamage }
                targets[g] = possibleTargets.maxWithOrNull(compareBy<Group> { it.effectivePower }.thenBy { it.initiative })!!
                remainingTarget.remove(targets[g])
            }
        }
        
        for (g in targets.keys.sortedByDescending { it.initiative }) {
            if (g.units > 0) {
                val target = targets[g]!!
                val damage = g.damageDealtTo(target)
                if (damage > 0 && target.units > 0) {
                    val dies = damage / target.hp
                    target.units = max(0, target.units - dies)
                    if (dies > 0) {
                        attack = true
                    }
                }
            }
        }
        
        army = army.filter { it.units > 0 }
    }
    
    return Pair(army.all { it.immuneSystem }, army.sumOf { it.units })
}

fun evaluatorOne(input: List<String>): Long = fight(input, 0).second

fun evaluatorTwo(input: List<String>): Long {
    var l = 0
    var h = Int.MAX_VALUE / 2
    
    while (h - l > 1) {
        val m = (h + l) / 2
        if (fight(input, m).first) {
            h = m
        } else {
            l = m
        }
    }
    
    return fight(input, h).second
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