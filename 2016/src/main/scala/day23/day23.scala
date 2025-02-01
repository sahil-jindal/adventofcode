package day23

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

def solve(input: Array[String], a: Int): Int = {
    val prg = input.map(_.split(' ')).toArray
    var regs = Map[String, Int]("a" -> a)
    var ip = 0

    def getReg(reg: String): Int =
        reg.toIntOption.getOrElse(regs.getOrElse(reg, 0))

    def setReg(reg: String, value: Int): Unit =
        if (reg.toIntOption.isEmpty) regs(reg) = value

    while ip < prg.length do
        prg(ip) match
            case Array("cpy", x, y) => 
                setReg(y, getReg(x)); 
                ip += 1
            case Array("inc", x) => 
                setReg(x, getReg(x) + 1); 
                ip += 1
            case Array("mul", x, y) => 
                setReg(y, getReg(x) * getReg(y)); 
                ip += 1
            case Array("dec", x) => 
                setReg(x, getReg(x) - 1);
                ip += 1
            case Array("jnz", x, y) => 
                ip += (if getReg(x) != 0 then getReg(y) else 1)
            case Array("tgl", x) => {
                val ipDst = ip + getReg(x)

                if ipDst >= 0 && ipDst < prg.length then
                    prg(ipDst)(0) = prg(ipDst)(0) match
                        case "cpy" => "jnz"
                        case "inc" => "dec"
                        case "dec" => "inc"
                        case "jnz" => "cpy"
                        case "tgl" => "inc"
                        case other  => other
                
                ip += 1
            }
            case stm => throw new Exception(s"Cannot parse ${stm.mkString(" ")}")

    getReg("a")
}

def evaluatorOne(input: Array[String]): Long = solve(input, 7)
def evaluatorTwo(input: Array[String]): Long = solve(input, 12)

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

@main
def hello(): Unit =
    readLinesFromFile("day23.txt") match
        case Success(lines) => {
            val input = lines.toArray
            input(5) = "cpy c a";
            input(6) = "mul d a";
            input(7) = "cpy 0 d";
            input(8) = "cpy 0 c";
            
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
