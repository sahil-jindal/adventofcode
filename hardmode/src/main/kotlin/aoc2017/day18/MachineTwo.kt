package aoc2017.day18

import java.util.LinkedList

class MachineTwo(p: Long, private val qIn: LinkedList<Long>, private val qOut: LinkedList<Long>) :
    Machine<Pair<Boolean, Int>>() {

    private var valueSent = 0

    init {
        setReg("p", p)
    }

    override fun state(): Pair<Boolean, Int> = Pair(running, valueSent)

    override fun snd(reg: String) {
        qOut.add(getReg(reg))
        valueSent++
        ip++
    }

    override fun rcv(reg: String) {
        if (qIn.isNotEmpty()) {
            setReg(reg, qIn.removeFirst())
            ip++
        } else {
            running = false
        }
    }
}