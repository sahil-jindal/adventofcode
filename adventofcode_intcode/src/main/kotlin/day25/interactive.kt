package day25

import intcode.IntCodeMachine
import java.io.File

fun main() {
    val program = File("src/main/resources/day25.txt").readText().trim()
    val machine = IntCodeMachine(program)
    machine.runInteractive()
}