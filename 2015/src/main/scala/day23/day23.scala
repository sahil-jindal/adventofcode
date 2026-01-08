package day23

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

sealed trait Op
case object Hlf extends Op
case object Tpl extends Op
case object IncA extends Op
case object IncB extends Op
case class Jmp(a: Int) extends Op
case class Jie(a: Int) extends Op
case class Jio(a: Int) extends Op

def parseInput(input: List[String]) = input.collect {
    case "hlf a" => Hlf
    case "tpl a" => Tpl
    case "inc a" => IncA
    case "inc b" => IncB
    case s"jmp $num" => Jmp(num.toInt)
    case s"jie a, $num" => Jie(num.toInt)
    case s"jio a, $num" => Jio(num.toInt)
}

def Solve(prg: List[Op], start: Int): Int = {
    var (a, b) = (start, 0)
    var ip = 0
    
    while (ip >= 0 && ip < prg.length) {
        prg(ip) match {
            case Hlf => a /= 2; ip += 1
            case Tpl => a *= 3; ip += 1
            case IncA => a += 1; ip += 1
            case IncB => b += 1; ip += 1
            case Jmp(x) => ip += x
            case Jie(x) => ip += (if (a % 2 == 0) x else 1)
            case Jio(x) => ip += (if (a == 1) x else 1)
        }
    }

    return b
}

def evaluatorOne(input: List[Op]): Int = Solve(input, 0)
def evaluatorTwo(input: List[Op]): Int = Solve(input, 1)

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