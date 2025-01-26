package daytwentythree

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

def Solve(input: List[String], a: Long): Long = {
    val regs = Map[String, Long]().withDefaultValue(0L)
    var ip = 0L

    def getReg(reg: String): Long = {
        reg.toLongOption.getOrElse(regs(reg))
    }

    def setReg(reg: String, value: Long): Unit = {
        regs(reg) = value
    }

    setReg("a", a)
    
    while (ip >= 0 && ip < input.length) {
        val line = input(ip.toInt)
        val parts = line.replace(",", "").split(" ")
        
        parts(0) match {
            case "hlf" =>
                setReg(parts(1), getReg(parts(1)) / 2)
                ip += 1
            case "tpl" =>
                setReg(parts(1), getReg(parts(1)) * 3)
                ip += 1
            case "inc" =>
                setReg(parts(1), getReg(parts(1)) + 1)
                ip += 1
            case "jmp" =>
                ip += parts(1).toLong
            case "jie" =>
                ip += (if (getReg(parts(1)) % 2 == 0) parts(2).toLong else 1)
            case "jio" =>
                ip += (if (getReg(parts(1)) == 1) parts(2).toLong else 1)
            case _ =>
                throw new Exception(s"Cannot parse $line")
        }
    }

    regs("b")
}

def evaluatorOne(input: List[String]): Long = Solve(input, 0)
def evaluatorTwo(input: List[String]): Long = Solve(input, 1)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

@main
def hello(): Unit =
    readLinesFromFile("daytwentythree.txt") match
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }