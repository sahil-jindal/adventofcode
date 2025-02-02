package day25

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

def parseInput(input: List[String]) = input.map(_.split(' ')).toArray

def solve(prg: Array[Array[String]], a: Int) = {
    var regs = Map[String, Int]("a" -> a)
    var ip = 0

    def getReg(reg: String): Int =
        reg.toIntOption.getOrElse(regs.getOrElse(reg, 0))

    def setReg(reg: String, value: Int): Unit =
        if (reg.toIntOption.isEmpty) regs(reg) = value

    Iterator.continually {
        if (ip >= prg.length) None
        
        else prg(ip) match
            case Array("cpy", x, y) => 
                setReg(y, getReg(x)); 
                ip += 1; 
                None
            case Array("inc", x) => 
                setReg(x, getReg(x) + 1); 
                ip += 1; 
                None
            case Array("out", x) => 
                ip += 1; 
                Some(getReg(x))
            case Array("dec", x) => 
                setReg(x, getReg(x) - 1); 
                ip += 1; 
                None
            case Array("jnz", x, y) => 
                ip += (if getReg(x) != 0 then getReg(y) else 1); 
                None
            case _ => 
                throw new Exception(s"Cannot parse ${prg(ip).mkString(" ")}")

    }.collect { case Some(bit) => bit }
}

def evaluatorOne(input: List[String]) =  {
    val prg = parseInput(input)

    Iterator.from(0).find { a =>
        //println(s"Testing $a")
        val expectedBit = Iterator.iterate(0)(_ ^ 1)
        solve(prg, a).take(100).corresponds(expectedBit)(_ == _)
    }.get

    // var low = 0
    // var high = 4000000 // Arbitrary high value, adjust as needed
    
    // while (low < high) {
    //     println(s"low: $low and high: $high")
    //     // val mid = (low + high) / 2
    //     val mid = low + (high - low) / 2
    //     val expectedBit = Iterator.iterate(0)(_ ^ 1)
    //     if (solve(prg, mid).take(100).corresponds(expectedBit)(_ == _)) high = mid
    //     else low = mid + 1
    // }
    //
    // low
}



def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

@main
def hello(): Unit =
    readLinesFromFile("day25.txt") match
        case Success(lines) => {
            // val input = lines.toArray
            // input(5) = "cpy c a";
            // input(6) = "mul d a";
            // input(7) = "cpy 0 d";
            // input(8) = "cpy 0 c";
            
            println(s"Part One: ${evaluatorOne(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
