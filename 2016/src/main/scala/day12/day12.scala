package day12

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

def Solve(input: List[String], c: Int): Int = {
    val prg = input.map(_.split(' ')).toArray
    val regs = Map[String, Int]("c" -> c)
    var ip = 0

    def getReg(reg: String): Int =
        reg.toIntOption.getOrElse(regs.getOrElse(reg, 0))

    def setReg(reg: String, value: Int): Unit = 
        regs(reg) = value
    
    while (ip >= 0 && ip < input.length) {
        prg(ip) match {
            case Array("cpy", x, y) =>
                setReg(y, getReg(x))
                ip += 1
            case Array("inc", x) =>
                setReg(x, getReg(x) + 1)
                ip += 1
            case Array("dec", x) =>
                setReg(x, getReg(x) - 1)
                ip += 1
            case Array("jnz", x, y) =>
                ip += (if (getReg(x) != 0) then getReg(y) else 1)
            case _ =>
                println(s"Cannot parse ${prg(ip)}")
        }
    }

    getReg("a")
}

def evaluatorOne(input: List[String]): Long = Solve(input, 0)
def evaluatorTwo(input: List[String]): Long = Solve(input, 1)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit =
    readLinesFromFile("day12.txt") match
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
