package day23

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

def evaluatorOne(input: List[String]): Int = {
    val prog = input.map(_.split(" "))
    val regs = Map[String, Int]()
    var ip = 0
    var mulCount = 0

    def getReg(reg: String): Int =
        reg.toIntOption.getOrElse(regs.getOrElse(reg, 0))

    def setReg(reg: String, value: Int): Unit =
        regs(reg) = value

    while (ip >= 0 && ip < prog.length) {
        prog(ip) match {
            case Array("set", x, y) => {
                setReg(x, getReg(y))
                ip += 1
            }
            case Array("sub", x, y) => {
                setReg(x, getReg(x) - getReg(y))
                ip += 1
            }
            case Array("mul", x, y) => {
                mulCount += 1
                setReg(x, getReg(x) * getReg(y))
                ip += 1
            }
            case Array("jnz", x, y) => {
                ip += (if (getReg(x) != 0) getReg(y) else 1)
            }
            case _ => throw new Exception(s"Cannot parse ${prog(ip)}")
        }
    }

    return mulCount
}

def isPrime(n: Int): Boolean = {
    if n < 2 then return false
    return (2 to math.sqrt(n).toInt).forall(n % _ != 0)
}

def evaluatorTwo(): Int = (109300 to 126300 by 17).count(!isPrime(_))

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day23.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo()}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}