package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

sealed trait Bit
case object Unknown extends Bit
case class Value(a: Boolean) extends Bit

case class Command(baseAddr: Int, value: Long)
case class ProgramSegment(mask: IndexedSeq[Bit], commands: List[Command])

def parseBinaryToLong(str: IndexedSeq[Bit]): Long = {
    require(str.forall(_.isInstanceOf[Value]))
    return str.reverse.zipWithIndex.collect { case (Value(true), idx) => 1L << idx }.sum
}

def groupCommands(input: List[String]): List[List[String]] = {
    return input.foldLeft(List.empty[List[String]]) {
        case (Nil, line) if line.startsWith("mask") => List(List(line))
        case (acc, line) if line.startsWith("mask") => acc :+ List(line)
        case (acc, line) => acc.init :+ (acc.last :+ line)
    }
}

def parseInput(input: List[String]): List[ProgramSegment] = {
    return groupCommands(input).map(group => {
        val mask = group.head.stripPrefix("mask = ").collect {
            case '0' => Value(false)
            case '1' => Value(true)
            case 'X' => Unknown
        }

        val commands = group.tail.map(line => {
            val List(a, b) = raw"(\d+)".r.findAllIn(line).map(_.toInt).toList
            Command(a, b.toLong)
        })

        ProgramSegment(mask, commands)
    })
}

def addresses(baseAddr: Long, mask: IndexedSeq[Bit]): List[Long] = {
    return mask.zipWithIndex.foldLeft(List(0L)) { case (prefixes, (ch, i)) =>
        val bit = (baseAddr >> (35 - i)) & 1

        ch match {
            case Value(false) => prefixes.map(prefix => (prefix << 1) + bit)
            case Value(true) => prefixes.map(prefix => (prefix << 1) + 1)
            case Unknown => prefixes.flatMap(prefix => List((prefix << 1), (prefix << 1) + 1))
        }
    }    
}

def evaluatorOne(input: List[ProgramSegment]): Long = {
    val mem = Map.empty[Int, Long]
    
    for {
        ProgramSegment(mask, commands) <- input
        andMask = parseBinaryToLong(mask.map(it => if (it == Unknown) Value(true) else it))
        orMask = parseBinaryToLong(mask.map(it => if (it == Unknown) Value(false) else it))
        Command(baseAddr, value) <- commands
    } mem(baseAddr) = (value & andMask) | orMask
    
    return mem.values.sum
}

def evaluatorTwo(input: List[ProgramSegment]): Long = {
    val mem = Map.empty[Long, Long]
    
    for {
        ProgramSegment(mask, commands) <- input
        Command(baseAddr, value) <- commands
        addr <- addresses(baseAddr, mask)
    } mem(addr) = value
    
    return mem.values.sum
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day14.txt") match {
        case Success(lines) => {
            val input = parseInput(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}