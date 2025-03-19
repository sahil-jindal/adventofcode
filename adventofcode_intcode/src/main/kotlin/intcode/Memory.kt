package intcode

class Memory(private val initial: LongArray) {
    private val mem = mutableMapOf<Long, Long>()

    private constructor(initial: LongArray, mem: Map<Long, Long>) : this(initial.copyOf()) {
        this.mem.putAll(mem)
    }

    operator fun get(addr: Long): Long =
        mem[addr] ?: if (addr in initial.indices.map { it.toLong() }) initial[addr.toInt()] else 0L

    operator fun set(addr: Long, value: Long) {
        mem[addr] = value
    }

    val length: Long get() = mem.keys.maxOrNull()?.coerceAtLeast(initial.size.toLong()) ?: initial.size.toLong()

    fun clone() = Memory(initial, mem)
    fun reset() = mem.clear()
}