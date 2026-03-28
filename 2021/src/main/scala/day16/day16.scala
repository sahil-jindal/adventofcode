package day16

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer

sealed trait Packet
case class Literal(version: Int, payload: Long) extends Packet
case class Operator(version: Int, ptype: Int, packets: Vector[Packet]) extends Packet

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
        List(8, 4, 2, 1).map(mask => (value & mask) != 0)
    })
)

def getPacket(reader: BitSequenceReader): Packet = {
    val version = reader.readInt(3)
    val ptype = reader.readInt(3)

    if (ptype == 4) {
        var continue = true
        var payload = 0L

        while (continue) {
            continue = reader.readInt(1) == 1
            payload = (payload << 4) + reader.readInt(4)
        }

        return Literal(version, payload)
    } 
    
    val packets = ListBuffer.empty[Packet]
    
    if (reader.readInt(1) == 0) {
        val length = reader.readInt(15)
        val subPackages = reader.getBitSequenceReader(length)
        while (subPackages.nonEmpty) { packets += getPacket(subPackages) }
    } else {
        val packetCount = reader.readInt(11)
        packets ++= (for (_ <- 0 until packetCount) yield getPacket(reader))
    }

    return Operator(version, ptype, packets.toVector)
}

def evaluatorOne(packet: Packet): Int = packet match {
    case Literal(version, _) => version 
    case Operator(version, _, packets) => {
        version + packets.map(evaluatorOne).sum
    }
}

def evaluatorTwo(packet: Packet): Long = packet match {
    case Literal(_, payload) => payload
    case Operator(_, ptype, packets) => {
        val parts = packets.map(evaluatorTwo)

        ptype match {
            case 0 => parts.sum
            case 1 => parts.product
            case 2 => parts.min
            case 3 => parts.max
            case 5 => if parts(0) > parts(1) then 1 else 0
            case 6 => if parts(0) < parts(1) then 1 else 0
            case 7 => if parts(0) == parts(1) then 1 else 0
        }
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