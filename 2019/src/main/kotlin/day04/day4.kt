package day04

fun split(st: String): Sequence<String> = sequence {
    var index = 0
    while (index < st.length) {
        val sequence = Regex("[" + st[index] + "]+").find(st.substring(index))?.value ?: ""
        yield(sequence)
        index += sequence.length
    }
}

fun ok(password: String, tripletsAllowed: Boolean): Boolean {
    if (password.zipWithNext().any { (a, b) -> a > b }) return false
    return split(password).any { it.length >= 2 && (tripletsAllowed || it.length == 2) }
}

fun solve(input: String, tripletsAllowed: Boolean): Int {
    val (start, end) = input.split("-").map { it.toInt() }
    return (start..end).count { ok(it.toString(), tripletsAllowed) }
}

fun evaluatorOne(input: String): Int = solve(input, true)
fun evaluatorTwo(input: String): Int = solve(input, false)

fun main() {
    val input = "145852-616942"
    println("Part One: ${evaluatorOne(input)}")
    println("Part Two: ${evaluatorTwo(input)}")
}