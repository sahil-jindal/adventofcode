package intcode

data class IntCodeOutput(val output: List<Long>) : List<Long> by output {
    fun toAscii(): String = output.joinToString("") {
        if (it in 0..255) it.toInt().toChar().toString() else it.toString()
    }
}