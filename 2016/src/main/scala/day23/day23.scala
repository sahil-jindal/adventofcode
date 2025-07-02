package day23

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

def parseInput(input: Array[String]) = input.map(_.split(' ')).toList

def solve(input: List[Array[String]], a: Int): Int = {
    val prg = input.map(_.clone())
    var regs = Map("a" -> a)
    var ip = 0

    def getReg(reg: String): Int = reg.toIntOption.getOrElse(regs.getOrElse(reg, 0))
    def setReg(reg: String, value: Int): Unit = if (reg.toIntOption.isEmpty) regs(reg) = value

    while (ip >= 0 && ip < prg.length) do {
        prg(ip) match {
            case Array("inc", x) => setReg(x, getReg(x) + 1); ip += 1
            case Array("dec", x) => setReg(x, getReg(x) - 1); ip += 1
            case Array("cpy", x, y) => setReg(y, getReg(x)); ip += 1
            case Array("mul", x, y) => setReg(y, getReg(x) * getReg(y)); ip += 1
            case Array("jnz", x, y) => ip += (if getReg(x) != 0 then getReg(y) else 1)
            case Array("tgl", x) => {
                val ipDst = ip + getReg(x)

                if (ipDst >= 0 && ipDst < prg.length) {
                    prg(ipDst)(0) = prg(ipDst)(0) match {
                        case "cpy" => "jnz"
                        case "inc" => "dec"
                        case "dec" => "inc"
                        case "jnz" => "cpy"
                        case "tgl" => "inc"
                        case other => other
                    }
                }
                
                ip += 1
            }
            case stm => throw new Exception(s"Cannot parse ${stm.mkString(" ")}")
        }
    }

    return getReg("a")
}

def evaluatorOne(input: List[Array[String]]): Long = solve(input, 7)
def evaluatorTwo(input: List[Array[String]]): Long = solve(input, 12)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day23.txt") match {
        case Success(lines) => {
            val newLines = lines.toArray
            newLines(5) = "cpy c a"
            newLines(6) = "mul d a"
            newLines(7) = "cpy 0 d"
            newLines(8) = "cpy 0 c"
            
            val input = parseInput(newLines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}