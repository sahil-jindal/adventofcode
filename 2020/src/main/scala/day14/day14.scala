package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

case class Command(baseAddr: Int, value: Long)
case class ProgramSegment(mask: String, commands: List[Command])

def parseBinaryToLong(str: String): Long = {
    if (!str.matches("^[01]+$")) throw new NumberFormatException(str)
    return str.map(_.asDigit).foldLeft(0L) { case (acc, item) => 2 * acc + item }
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
        val mask = group.head.stripPrefix("mask = ")

        val commands = group.tail.map(line => {
            val List(a, b) = raw"(\d+)".r.findAllIn(line).map(_.toInt).toList
            Command(a, b.toLong)
        })

        ProgramSegment(mask, commands)
    })
}

def addresses(baseAddr: Long, mask: String): List[Long] = {
    return mask.zipWithIndex.foldLeft(List(0L)) { case (prefixes, (ch, i)) =>
        val bit = (baseAddr >> (35 - i)) & 1

        ch match {
            case '0' => prefixes.map(prefix => (prefix << 1) + bit)
            case '1' => prefixes.map(prefix => (prefix << 1) + 1)
            case  _  => prefixes.flatMap(prefix => List((prefix << 1), (prefix << 1) + 1))
        }
    }    
}

def evaluatorOne(input: List[ProgramSegment]): Long = {
    val mem = Map.empty[Int, Long]
    
    for {
        ProgramSegment(mask, commands) <- input
        andMask = parseBinaryToLong(mask.replace("X", "1"))
        orMask = parseBinaryToLong(mask.replace("X", "0"))
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