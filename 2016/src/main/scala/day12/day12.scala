package day12

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

def parseInput(input: List[String]) = input.map(_.split(' '))

def solve(prg: List[Array[String]], c: Int): Int = {
    val regs = Map("c" -> c)
    var ip = 0

    def getReg(reg: String): Int = reg.toIntOption.getOrElse(regs.getOrElse(reg, 0))
    def setReg(reg: String, value: Int): Unit = regs(reg) = value
    
    while (ip >= 0 && ip < prg.length) {
        prg(ip) match {
            case Array("cpy", x, y) => setReg(y, getReg(x)); ip += 1
            case Array("inc", x) => setReg(x, getReg(x) + 1); ip += 1
            case Array("dec", x) => setReg(x, getReg(x) - 1); ip += 1
            case Array("jnz", x, y) => ip += (if (getReg(x) != 0) then getReg(y) else 1)
            case _ => throw new Exception(s"Cannot parse ${prg(ip)}")
        }
    }

    return getReg("a")
}

def evaluatorOne(input: List[Array[String]]): Int = solve(input, 0)
def evaluatorTwo(input: List[Array[String]]): Int = solve(input, 1)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day12.txt") match {
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