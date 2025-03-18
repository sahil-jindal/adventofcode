package day23

import intcode.IntCodeMachine
import java.io.File

data class Packet(val address: Long, val x: Long, val y: Long)

typealias Packets = MutableList<Packet>

fun receive(packets: Packets, address: Int): Pair<MutableList<Long>, Packets> {
    val filteredPackets = mutableListOf<Packet>()
    val data = mutableListOf<Long>()

    for (packet in packets) {
        if (packet.address == address.toLong()) {
            data.add(packet.x)
            data.add(packet.y)
        } else {
            filteredPackets.add(packet)
        }
    }

    return Pair(data, filteredPackets)
}

fun nic(program: String, address: Int): (Packets) -> Packets {
    val icm = IntCodeMachine(program)
    var output = icm.run(address.toLong())

    assert(output.isEmpty())

    return { input ->
        val (data, packets) = receive(input, address)

        if (data.isEmpty()) {
            data.add(-1)
        }

        output = icm.run(*data.toLongArray())

        for (d in output.indices step 3) {
            packets.add(Packet(output[d], output[d + 1], output[d + 2]))
        }

        packets
    }
}

fun nat(address: Int): (Packets) -> Packets {
    var yLastSent: Long? = null
    var x: Long? = null
    var y: Long? = null

    return { input ->
        val (data, packets) = receive(input, address)

        if (data.isNotEmpty()) {
            x = data[data.size - 2]
            y = data[data.size - 1]
        }

        if (packets.isEmpty()) {
            assert(x != null && y != null)
            packets.add(Packet(if (y == yLastSent) 255 else 0, x!!, y!!))
            yLastSent = y
        }

        packets
    }
}

fun solve(input: String, hasNat: Boolean): Long {
    val machines = (0 until 50).map { address -> nic(input, address) }.toMutableList()
    val natAddress = 255

    if (hasNat) {
        machines.add(nat(natAddress))
    }

    var packets = mutableListOf<Packet>()

    while (packets.none { it.address == natAddress.toLong() }) {
        machines.forEach { machine -> packets = machine(packets) }
    }

    return packets.single { it.address == natAddress.toLong() }.y
}

fun evaluatorOne(input: String) = solve(input, false)
fun evaluatorTwo(input: String) = solve(input, true)

fun readLinesFromFile(filePath: String): Result<List<String>> =
    runCatching { File(filePath).readLines() }

fun main() {
    readLinesFromFile("src/main/resources/day23.txt")
        .onSuccess {
            val line = it.first()
            println("Part One: ${evaluatorOne(line)}")
            println("Part Two: ${evaluatorTwo(line)}")
        }
        .onFailure {
            println("Error reading file: ${it.message}")
        }
}