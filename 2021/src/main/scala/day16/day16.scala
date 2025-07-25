package day16

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer

case class Packet(version: Int, ptype: Int, payload: Long, packets: List[Packet])

case class BitSequenceReader(private val bits: IndexedSeq[Boolean]) {
    private var ptr = 0

    def nonEmpty: Boolean = ptr < bits.length

    def getBitSequenceReader(bitCount: Int): BitSequenceReader = {
        val subBits = bits.slice(ptr, ptr + bitCount)
        ptr += bitCount
        return BitSequenceReader(subBits)
    }

    def readInt(bitCount: Int): Int = {
        val subBits = bits.slice(ptr, ptr + bitCount)
        ptr += bitCount
        return subBits.reverse.zipWithIndex.collect { case (true, i) => 1 << i }.sum
    }
}

def getReader(input: String) = BitSequenceReader(
    input.flatMap(hexChar => {
        val value = Integer.parseInt(hexChar.toString, 16)
        Seq(8, 4, 2, 1).map(mask => (value & mask) != 0)
    })
)

def getPacket(reader: BitSequenceReader): Packet = {
    val version = reader.readInt(3)
    val ptype = reader.readInt(3)
    val packets = ListBuffer.empty[Packet]
    var payload = 0L

    if (ptype == 4) {
        var continue = true
        while (continue) {
            val isLast = reader.readInt(1) == 0
            payload = payload * 16 + reader.readInt(4)
            continue = !isLast
        }
    } else if (reader.readInt(1) == 0) {
        val length = reader.readInt(15)
        val subPackages = reader.getBitSequenceReader(length)
        while (subPackages.nonEmpty) {
            packets += getPacket(subPackages)
        }
    } else {
        val packetCount = reader.readInt(11)
        packets ++= (for (_ <- 0 until packetCount) yield getPacket(reader))
    }

    return Packet(version, ptype, payload, packets.toList)
}

def evaluatorOne(packet: Packet): Int = {
    return packet.version + packet.packets.map(evaluatorOne).sum
}

def evaluatorTwo(packet: Packet): Long = {
    val parts = packet.packets.map(evaluatorTwo)

    return packet.ptype match {
        case 0 => parts.sum
        case 1 => parts.product
        case 2 => parts.min
        case 3 => parts.max
        case 4 => packet.payload
        case 5 => if parts(0) > parts(1) then 1 else 0
        case 6 => if parts(0) < parts(1) then 1 else 0
        case 7 => if parts(0) == parts(1) then 1 else 0
    }
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day16.txt") match {
        case Success(lines) => {
            val input = getPacket(getReader(lines.head))
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}