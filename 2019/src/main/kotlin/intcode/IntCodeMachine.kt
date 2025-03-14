package intcode

import java.util.*
import kotlin.math.min

class IntCodeMachine(program: String) {
    var memory: Memory
    val inputQueue = ArrayDeque<Long>()
    private var ip: Long = 0
    private var bp: Long = 0
    private val modeMask = listOf(0, 100, 1000, 10000)

    init {
        memory = Memory(program.split(",").filter { it.isNotEmpty() }.map { it.toLong() }.toLongArray())
    }

    private constructor(memory: Memory, ip: Long, bp: Long, input: Queue<Long>) : this("") {
        this.memory = memory.clone()
        this.ip = ip
        this.bp = bp
        this.inputQueue.addAll(input)
    }

    fun reset() {
        memory.reset()
        ip = 0
        bp = 0
        inputQueue.clear()
    }

    fun clone() = IntCodeMachine(memory, ip, bp, inputQueue)
    fun halted() = getOpcode(ip) == Opcode.Hlt

    private fun getMode(addr: Long, i: Int): Mode {
        val divisor = modeMask[i].toLong()
        return Mode.entries[(memory[addr] / divisor % 10).toInt()]
    }

    private fun getOpcode(addr: Long) =
        Opcode.fromValue((memory[addr] % 100).toInt())

    fun runInteractive() {
        var input = longArrayOf()
        while (true) {
            val output = run(*input)
            print(output.toAscii())
            if (halted()) break
            input = readlnOrNull()?.plus("\n")?.let { asciiEncode(it) } ?: longArrayOf()
        }
    }

    private fun asciiEncode(s: String) = s.map { it.code.toLong() }.toLongArray()

    fun run(): IntCodeOutput {
        return run(0)
    }

    fun run(vararg input: String) = run(*asciiEncode(input.joinToString("\n") + "\n"))

    fun run(vararg input: Long): IntCodeOutput {
        input.forEach { inputQueue.add(it) }
        val output = mutableListOf<Long>()

        while (true) {
            val opcode = getOpcode(ip)
            val oldIp = ip

            fun addr(i: Int): Long = when (getMode(oldIp, i)) {
                Mode.Positional -> memory[oldIp + i]
                Mode.Immediate -> oldIp + i
                Mode.Relative -> bp + memory[oldIp + i]
            }

            fun arg(i: Int) = memory[addr(i)]

            when (opcode) {
                Opcode.Add -> {
                    memory[addr(3)] = arg(1) + arg(2)
                    ip += 4
                }
                Opcode.Mul -> {
                    memory[addr(3)] = arg(1) * arg(2)
                    ip += 4
                }
                Opcode.In -> {
                    if (inputQueue.isNotEmpty()) {
                        memory[addr(1)] = inputQueue.removeFirst()
                        ip += 2
                    } else {
                        // Wait for input
                        return IntCodeOutput(output)
                    }
                }
                Opcode.Out -> {
                    output.add(arg(1))
                    ip += 2
                }
                Opcode.Jnz -> ip = if (arg(1) != 0L) arg(2) else ip + 3
                Opcode.Jz -> ip = if (arg(1) == 0L) arg(2) else ip + 3
                Opcode.Lt -> {
                    memory[addr(3)] = if (arg(1) < arg(2)) 1 else 0
                    ip += 4
                }
                Opcode.Eq -> {
                    memory[addr(3)] = if (arg(1) == arg(2)) 1 else 0
                    ip += 4
                }
                Opcode.StR -> {
                    bp += arg(1)
                    ip += 2
                }
                Opcode.Hlt -> return IntCodeOutput(output)
            }

            if (ip == oldIp) break
        }

        return IntCodeOutput(output)
    }

    fun disassemble(trace: Boolean = false, count: Int = Int.MAX_VALUE, startIp: Long = -1): String {
        var currentIp = if (startIp == -1L) ip else startIp
        val sb = StringBuilder()
        val maxCount = min(count, (memory.length - currentIp).toInt())

        fun formatValue(value: Long): String {
            return if (value in 32..126) "$value '${value.toInt().toChar()}'" else value.toString()
        }

        repeat(maxCount) {
            try {
                sb.append(currentIp.toString().padStart(4, '0') + "  ")
                val opcode = getOpcode(currentIp)

                fun addr(i: Int): String {
                    val offset = currentIp + i
                    return when (getMode(currentIp, i)) {
                        Mode.Positional -> "mem[${memory[offset]}]"
                        Mode.Immediate -> memory[offset].toString()
                        Mode.Relative -> {
                            val offsetVal = memory[offset]
                            when {
                                offsetVal > 0 -> "mem[bp + $offsetVal]"
                                offsetVal == 0L -> "mem[bp]"
                                else -> "mem[bp - ${-offsetVal}]"
                            }
                        }
                    }
                }

                fun arg(i: Int): String {
                    val addrStr = addr(i)
                    return if (trace) {
                        val value = when (getMode(currentIp, i)) {
                            Mode.Positional -> memory[memory[currentIp + i]]
                            Mode.Immediate -> memory[currentIp + i]
                            Mode.Relative -> memory[bp + memory[currentIp + i]]
                        }
                        "$addrStr (${formatValue(value)})"
                    } else {
                        addrStr
                    }
                }

                when (opcode) {
                    Opcode.Add -> {
                        val dest = addr(3)
                        val a1 = arg(1)
                        val a2 = arg(2)
                        sb.appendLine("$dest = $a1 + $a2;")
                        currentIp += 4
                    }
                    // Similar handling for other opcodes...
                    else -> {
                        sb.appendLine(memory[currentIp].toString())
                        currentIp++
                    }
                }
            } catch (e: Exception) {
                sb.appendLine(memory[currentIp].toString())
                currentIp++
            }
        }
        return sb.toString().trimEnd()
    }
}