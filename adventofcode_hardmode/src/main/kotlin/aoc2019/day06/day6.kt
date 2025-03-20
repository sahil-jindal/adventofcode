package aoc2019.day06

import java.io.File

typealias ChildToParent = Map<String, String>

fun parseTree(input: List<String>): ChildToParent =
    input.map { it.split(")") }.associate { (parent, child) -> child to parent }

fun getAncestors(childToParent: Map<String, String>, node: String): Sequence<String> = sequence {
    var parent = childToParent[node]
    while (parent != null) {
        yield(parent)
        parent = childToParent[parent]
    }
}

fun evaluatorOne(childToParent: ChildToParent): Int =
    childToParent.keys.sumOf { getAncestors(childToParent, it).count() }

fun evaluatorTwo(childToParent: ChildToParent): Int {
    val youAncestors = getAncestors(childToParent, "YOU").withIndex().associate { it.value to it.index }
    val sanAncestors = getAncestors(childToParent, "SAN").toList()

    sanAncestors.forEachIndexed { index, ancestor ->
        youAncestors[ancestor]?.let { return it + index }
    }

    error("No common ancestor found between YOU and SAN")
}

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/aoc2019/day06.txt")
        .onSuccess {
            val childToParent = parseTree(it)
            println("Part One: ${evaluatorOne(childToParent)}")
            println("Part Two: ${evaluatorTwo(childToParent)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}