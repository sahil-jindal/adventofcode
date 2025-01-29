package day12

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

def Solve(input: List[String], c: Int): Int = {
    val regs = Map[String, Int]().withDefaultValue(0)
    var ip = 0

    def getReg(reg: String): Int = {
        reg.toIntOption.getOrElse(regs(reg))
    }

    def setReg(reg: String, value: Int): Unit = {
        regs(reg) = value
    }

    setReg("c", c)
    
    while (ip >= 0 && ip < input.length) {
        val line = input(ip)
        val parts = line.split(' ')
        
        parts(0) match {
            case "cpy" =>
                setReg(parts(2), getReg(parts(1)))
                ip += 1
            case "inc" =>
                setReg(parts(1), getReg(parts(1)) + 1)
                ip += 1
            case "dec" =>
                setReg(parts(1), getReg(parts(1)) - 1)
                ip += 1
            case "jnz" =>
                ip += (if (getReg(parts(1)) != 0) then getReg(parts(2)) else 1)
            case _ =>
                throw new Exception(s"Cannot parse $line")
        }
    }

    regs("a")
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
