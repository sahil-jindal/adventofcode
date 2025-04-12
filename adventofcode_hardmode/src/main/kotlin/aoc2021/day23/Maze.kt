package aoc2021.day23

private const val columnMaskA = (1 shl 11) or (1 shl 15) or (1 shl 19) or (1 shl 23)
private const val columnMaskB = (1 shl 12) or (1 shl 16) or (1 shl 20) or (1 shl 24)
private const val columnMaskC = (1 shl 13) or (1 shl 17) or (1 shl 21) or (1 shl 25)
private const val columnMaskD = (1 shl 14) or (1 shl 18) or (1 shl 22) or (1 shl 26)

data class Maze(val a: Int, val b: Int, val c: Int, val d: Int) {

    companion object {
        fun parseInput(input: List<String>): Maze {
            var maze = Maze(columnMaskA, columnMaskB, columnMaskC, columnMaskD)

            for ((y, line) in input.withIndex()) {
                for ((x, ch) in line.withIndex()) {
                    maze = maze.setItem(Point(y, x), ch)
                }
            }

            return maze
        }
    }

    private fun bitFromPoint(pt: Point): Int {
        return when (pt.y to pt.x) {
            1 to 1 -> 1 shl 0
            1 to 2 -> 1 shl 1
            1 to 3 -> 1 shl 2
            1 to 4 -> 1 shl 3
            1 to 5 -> 1 shl 4
            1 to 6 -> 1 shl 5
            1 to 7 -> 1 shl 6
            1 to 8 -> 1 shl 7
            1 to 9 -> 1 shl 8
            1 to 10 -> 1 shl 9
            1 to 11 -> 1 shl 10

            2 to 3 -> 1 shl 11
            2 to 5 -> 1 shl 12
            2 to 7 -> 1 shl 13
            2 to 9 -> 1 shl 14

            3 to 3 -> 1 shl 15
            3 to 5 -> 1 shl 16
            3 to 7 -> 1 shl 17
            3 to 9 -> 1 shl 18

            4 to 3 -> 1 shl 19
            4 to 5 -> 1 shl 20
            4 to 7 -> 1 shl 21
            4 to 9 -> 1 shl 22

            5 to 3 -> 1 shl 23
            5 to 5 -> 1 shl 24
            5 to 7 -> 1 shl 25
            5 to 9 -> 1 shl 26

            else -> Int.MAX_VALUE
        }
    }

    private fun setItem(pt: Point, ch: Char): Maze {
        if (ch == '#') return this

        val bit = bitFromPoint(pt)

        if (bit == Int.MAX_VALUE) {
            return this
        }

        return when (ch) {
            '.' -> Maze(a and bit.inv(), b and bit.inv(), c and bit.inv(), d and bit.inv())
            'A' -> Maze(a or bit, b and bit.inv(), c and bit.inv(), d and bit.inv())
            'B' -> Maze(a and bit.inv(), b or bit, c and bit.inv(), d and bit.inv())
            'C' -> Maze(a and bit.inv(), b and bit.inv(), c or bit, d and bit.inv())
            'D' -> Maze(a and bit.inv(), b and bit.inv(), c and bit.inv(), d or bit)
            else -> throw Exception()
        }
    }

    fun itemAt(pt: Point): Char {
        val bit = bitFromPoint(pt)

        if (bit == Int.MAX_VALUE) return '#'
        if ((a and bit) != 0) return 'A'
        if ((b and bit) != 0) return 'B'
        if ((c and bit) != 0) return 'C'
        if ((d and bit) != 0) return 'D'

        return '.'
    }

    fun canMoveToDoor(xFrom: Int, xTo: Int): Boolean {
        fun step(pt: Point): Point {
            return if (xFrom < xTo) pt.right() else pt.left()
        }

        var pt = step(Point(1, xFrom))

        while (pt.x != xTo) {
            if (this.itemAt(pt) != '.') {
                return false
            }

            pt = step(pt)
        }

        return true
    }

    fun canEnterRoom(ch: Char): Boolean {
        return when (ch) {
            'A' -> (b and columnMaskA) == 0 && (c and columnMaskA) == 0 && (d and columnMaskA) == 0
            'B' -> (a and columnMaskB) == 0 && (c and columnMaskB) == 0 && (d and columnMaskB) == 0
            'C' -> (a and columnMaskC) == 0 && (b and columnMaskC) == 0 && (d and columnMaskC) == 0
            'D' -> (a and columnMaskD) == 0 && (b and columnMaskD) == 0 && (c and columnMaskD) == 0
            else -> throw Exception()
        }
    }

    fun move(from: Point, to: Point): Maze {
        return setItem(to, itemAt(from)).setItem(from, '.')
    }

    fun finishedColumn(x: Int): Boolean {
        return when (x) {
            3 -> a == columnMaskA
            5 -> b == columnMaskB
            7 -> c == columnMaskC
            9 -> d == columnMaskD
            else -> throw Exception()
        }
    }

    fun finished(): Boolean {
        return finishedColumn(3) && finishedColumn(5) && finishedColumn(7) && finishedColumn(9)
    }
}