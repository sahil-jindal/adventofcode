package aoc2024.day09

import java.io.File
import java.util.ArrayList

data class Block(var fileId: Int, var length: Int)

typealias Fs = ArrayList<Block>

fun parse(input: String): Fs {
    return ArrayList(input.mapIndexed { i, ch ->
        Block(if (i % 2 == 1) -1 else i / 2, ch - '0')
    })
}

fun compactFs(fs: ArrayList<Block>, fragmentsEnabled: Boolean): ArrayList<Block> {
    // Convert to array for faster processing
    var i = 0
    var j = fs.size - 1

    while (i < j) {
        // Skip used blocks at the beginning
        while (i < j && fs[i].fileId != -1) {
            i++
        }

        // Skip free blocks at the end
        while (i < j && fs[j].fileId == -1) {
            j--
        }

        if (i < j) {
            // Found a free block at i and a used block at j
            relocateBlock(fs, i, j, fragmentsEnabled)
            j--
        }
    }

    return fs
}

fun relocateBlock(blocks: ArrayList<Block>, startIdx: Int, jIdx: Int, fragmentsEnabled: Boolean) {
    var i = startIdx

    while (i < jIdx) {
        val iBlock = blocks[i]
        val jBlock = blocks[jIdx]

        when {
            iBlock.fileId != -1 -> {
                // Skip used blocks
                i++
            }
            iBlock.length == jBlock.length -> {
                // Perfect fit - swap the blocks
                blocks[i] = Block(jBlock.fileId, jBlock.length)
                blocks[jIdx] = Block(-1, jBlock.length)
                return
            }
            iBlock.length > jBlock.length -> {
                // Free block is larger - use part of it and create a remainder
                val diff = iBlock.length - jBlock.length
                blocks[i] = Block(jBlock.fileId, jBlock.length)
                blocks.add(i + 1, Block(-1, diff))
                val jAfter = if (i + 1 <= jIdx) jIdx + 1 else jIdx
                blocks[jAfter] = Block(-1, jBlock.length)
                return
            }
            fragmentsEnabled -> {
                // Free block is smaller but fragmentation allowed - partial move
                val diff = jBlock.length - iBlock.length
                blocks[i] = Block(jBlock.fileId, iBlock.length)
                blocks[jIdx] = Block(jBlock.fileId, diff)
                blocks.add(jIdx + 1, Block(-1, iBlock.length))
                i++
            }
            else -> {
                // Can't use this free block
                i++
            }
        }
    }
}

fun checksum(fs: Fs): Long {
    var res = 0L
    var position = 0

    for (block in fs) {
        repeat (block.length) {
            if (block.fileId != -1) {
                res += position * block.fileId
            }
            
            position++
        }
    }

    return res
}

fun evaluatorOne(input: String): Long = checksum(compactFs(parse(input), true))
fun evaluatorTwo(input: String): Long = checksum(compactFs(parse(input), false))

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/aoc2024/day09.txt")
        .onSuccess {
            val line = it.first()
            println("Part One: ${evaluatorOne(line)}")
            println("Part Two: ${evaluatorTwo(line)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}