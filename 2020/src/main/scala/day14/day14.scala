package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import java.lang.Long
import scala.collection.mutable.Map

def addresses(baseAddr: Long, mask: String, i: Int): Seq[Long] = {
    if (i == -1) return Seq(0)
    
    val prefixes = addresses(baseAddr, mask, i - 1)
    
    return mask(i) match {
        case '0' => prefixes.map(prefix => (prefix << 1) + ((baseAddr >> (35 - i)) & 1))
        case '1' => prefixes.map(prefix => (prefix << 1) + 1)
        case _   => prefixes.flatMap(prefix => Seq((prefix << 1), (prefix << 1) + 1))
    }    
}

def evaluatorOne(input: List[String]): Long = {
    val mem = Map.empty[Long, Long]
    var orMask = 0L
    var andMask = 0xffffffffffffffL
    
    for (line <- input) {
        if (line.startsWith("mask")) {
            val mask = line.split(" = ")(1)
            andMask = Long.parseLong(mask.replace("X", "1"), 2)
            orMask = Long.parseLong(mask.replace("X", "0"), 2)
        } else {
            val num = "\\d+".r.findAllIn(line).map(_.toLong).toArray
            mem(num(0)) = (num(1) & andMask) | orMask
        }
    }
    
    return mem.values.foldLeft(0L)(_ + _)
}

def evaluatorTwo(input: List[String]): Long = {
    val mem = Map.empty[Long, Long]
    var mask = ""
    
    for (line <- input) {
        if (line.startsWith("mask")) {
            mask = line.split(" = ")(1)
        } else {
            val num = "\\d+".r.findAllIn(line).map(_.toLong).toArray
            val (baseAddr, value) = (num(0), num(1))
            for (addr <- addresses(baseAddr, mask, 35)) {
                mem(addr) = value
            }
        }
    }

    return mem.values.foldLeft(0L)(_ + _)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day14.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}