package aoc2021.day23

data class Point(val y: Int, val x: Int) {
    fun below(): Point = Point(y + 1, x)
    fun left(): Point = Point(y, x - 1)
    fun right(): Point = Point(y, x + 1)
}