package intcode

enum class Opcode(val value: Int) {
    Add(1),
    Mul(2),
    In(3),
    Out(4),
    Jnz(5),
    Jz(6),
    Lt(7),
    Eq(8),
    StR(9),
    Hlt(99);

    companion object {
        fun fromValue(value: Int): Opcode = entries.first { it.value == value }
    }
}