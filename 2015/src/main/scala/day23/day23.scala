package day23

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

def parseInput(input: List[String]): List[Array[String]] = input.map(_.replace(",", "").split(" "))

def Solve(prg: List[Array[String]], a: Long): Long = {
    val regs = Map[String, Long]("a" -> a)
    var ip = 0L

    def getReg(reg: String): Long = reg.toLongOption.getOrElse(regs.getOrElse(reg, 0L))
    def setReg(reg: String, value: Long): Unit = regs(reg) = value
    
    while (ip >= 0 && ip < prg.length) {
        prg(ip.toInt) match {
            case Array("hlf", x) => setReg(x, getReg(x) / 2); ip += 1
            case Array("tpl", x) => setReg(x, getReg(x) * 3); ip += 1
            case Array("inc", x) => setReg(x, getReg(x) + 1); ip += 1
            case Array("jmp", x) => ip += x.toLong
            case Array("jie", x, y) => ip += (if (getReg(x) % 2 == 0) y.toLong else 1)
            case Array("jio", x, y) => ip += (if (getReg(x) == 1) y.toLong else 1)
            case _ => throw new Exception(s"Cannot parse: ${prg(ip.toInt)}")
        }
    }

    return getReg("b")
}

def evaluatorOne(input: List[Array[String]]): Long = Solve(input, 0)
def evaluatorTwo(input: List[Array[String]]): Long = Solve(input, 1)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day23.txt") match {
        case Success(lines) => {
            val instructions = parseInput(lines)
            println(s"Part One: ${evaluatorOne(instructions)}")
            println(s"Part Two: ${evaluatorTwo(instructions)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}