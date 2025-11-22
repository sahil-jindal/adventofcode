package aoc2017.day18

abstract class Machine<TState> {
    private val regs = mutableMapOf<String, Long>().withDefault { 0L }
    protected var running = false
    protected var ip = 0

    protected fun getReg(reg: String): Long = reg.toLongOrNull() ?: regs.getValue(reg)
    protected fun setReg(reg: String, value: Long) { regs[reg] = value }

    protected abstract fun state(): TState
    protected abstract fun snd(reg: String)
    protected abstract fun rcv(reg: String)

    fun execute(prog: List<List<String>>): Sequence<TState> = sequence {
        while (ip in prog.indices) {
            running = true
            val inst = prog[ip]

            when (inst[0]) {
                "snd" -> snd(inst[1])
                "rcv" -> rcv(inst[1])
                "set" -> { setReg(inst[1], getReg(inst[2])); ip++ }
                "add" -> { setReg(inst[1], getReg(inst[1]) + getReg(inst[2])); ip++ }
                "mul" -> { setReg(inst[1], getReg(inst[1]) * getReg(inst[2])); ip++ }
                "mod" -> { setReg(inst[1], getReg(inst[1]) % getReg(inst[2])); ip++ }
                "jgz" -> ip += if (getReg(inst[1]) > 0) getReg(inst[2]).toInt() else 1
                else -> throw Exception("Cannot parse instruction: $inst")
            }

            yield(state())
        }

        running = false
        yield(state())
    }
}