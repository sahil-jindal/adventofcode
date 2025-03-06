package intcode

class ImmutableIntCodeMachine(private val icm: IntCodeMachine) {
    constructor(program: String) : this(IntCodeMachine(program))

    fun run(vararg input: Long): Pair<ImmutableIntCodeMachine, IntCodeOutput> {
        val cloned = icm.clone()
        return Pair(ImmutableIntCodeMachine(cloned), cloned.run(*input))
    }

    fun run(vararg input: String): Pair<ImmutableIntCodeMachine, IntCodeOutput> {
        val cloned = icm.clone()
        return Pair(ImmutableIntCodeMachine(cloned), cloned.run(*input))
    }

    fun halted() = icm.halted()
}