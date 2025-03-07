package day08

import java.io.File

fun parseInput(input: String): List<List<Int>> = input.map { it - '0' }.chunked(6 * 25)

fun evaluatorOne(layers: List<List<Int>>): Int {
    val layer = layers.minBy { layer -> layer.count { it == 0 } }
    val ones = layer.count { it == 1 }
    val twos = layer.count { it == 2 }
    return ones * twos
}

fun evaluatorTwo(layers: List<List<Int>>): String {
    val img = CharArray(6 * 25) { ' ' }

    for (layer in layers.asReversed()) {
        for (i in img.indices) {
            img[i] = when (layer[i]) {
                0 -> ' '
                1 -> '#'
                else -> img[i]
            }
        }
    }

    return img.toList().chunked(25).joinToString("\n") { it.joinToString("") }
}

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day08.txt")
        .onSuccess {
            val layers = parseInput(it.first())
            println("Part One: ${evaluatorOne(layers)}")
            println("Part Two:\n${evaluatorTwo(layers)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}

