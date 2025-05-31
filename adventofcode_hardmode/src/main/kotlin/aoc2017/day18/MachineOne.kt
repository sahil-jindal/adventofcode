package aoc2017.day18

class MachineOne : Machine<Long?>() {
    private var sent: Long? = null
    private var received: Long? = null

    override fun state(): Long? = received

    override fun snd(reg: String) {
        sent = getReg(reg)
        ip++
    }

    override fun rcv(reg: String) {
        if (getReg(reg) != 0L) received = sent
        ip++
    }
}