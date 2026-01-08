package day08

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Set

sealed trait Stm
case class Nop(arg: Int) extends Stm
case class Acc(arg: Int) extends Stm
case class Jmp(arg: Int) extends Stm

case class Pair(acc: Int, terminated: Boolean)

def parseInput(input: List[String]) = input.collect {
    case s"nop $arg" => Nop(arg.toInt)
    case s"acc $arg" => Acc(arg.toInt)
    case s"jmp $arg" => Jmp(arg.toInt)
}

def run(program: List[Stm]): Pair = {
    var (ip, acc) = (0, 0)
    val seen = Set.empty[Int]

    while (true) {
        if (ip >= program.length) return Pair(acc, true)
        if (seen.contains(ip)) return Pair(acc, false)
      
        seen += ip
        program(ip) match {
            case Nop(_)  => ip += 1
            case Acc(arg) => acc += arg; ip += 1
            case Jmp(arg) => ip += arg
        }
    }

    throw new RuntimeException("Unreachable")
}

def patches(program: List[Stm]): List[List[Stm]] = {
    return program.zipWithIndex.collect { 
        case (Jmp(arg), idx) => program.updated(idx, Nop(arg)) 
        case (Nop(arg), idx) => program.updated(idx, Jmp(arg))
    }
}

def evaluatorOne(input: List[Stm]): Int = run(input).acc
def evaluatorTwo(input: List[Stm]): Int = patches(input).map(run).find(_.terminated).get.acc

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day08.txt") match {
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