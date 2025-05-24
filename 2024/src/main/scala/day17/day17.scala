package day17

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer

enum Opcode { case Adv, Bxl, Bst, Jnz, Bxc, Out, Bdv, Cdv }

def parseNums(st: String) = raw"(\d+)".r.findAllIn(st).map(_.toLong).toList

def parseInput(input: List[String]): (List[Long], List[Long]) = {
    val state = input.take(3).flatMap(parseNums)
    val program = parseNums(input.last)
    return (state, program)
}

def run(state: Array[Long], program: List[Long]): List[Long] = {
    def combo(op: Int) = if op < 4L then op.toLong else state(op - 4)
    
    val res = ListBuffer.empty[Long]
    var ip = 0

    while (ip < program.size) {
        (Opcode.fromOrdinal(program(ip).toInt), program(ip + 1).toInt) match {
            case (Opcode.Adv, op) => state(0) = state(0) >> combo(op).toInt
            case (Opcode.Bdv, op) => state(1) = state(0) >> combo(op).toInt
            case (Opcode.Cdv, op) => state(2) = state(0) >> combo(op).toInt
            case (Opcode.Bxl, op) => state(1) = state(1) ^ op
            case (Opcode.Bst, op) => state(1) = combo(op) % 8
            case (Opcode.Jnz, op) => ip = if state(0) == 0 then ip else op - 2
            case (Opcode.Bxc, op) => state(1) = state(1) ^ state(2)
            case (Opcode.Out, op) => res += combo(op) % 8
        }

        ip += 2
    }

    return res.toList
}

// Determines register A for the given output. The search works recursively and in 
// reverse order, starting from the last number to be printed and ending with the first.
def generateA(program: List[Long], output: List[Long]): List[Long] = {
    if (output.isEmpty) return List(0)

    return (for {
        ah <- generateA(program, output.tail)
        al <- 0 to 7 by 1
        a = ah * 8 + al
        if run(Array(a, 0, 0), program).sameElements(output)
    } yield a).toList
}

def evaluatorOne(input: List[String]): String = {
    val (state, program) = parseInput(input)
    return run(state.toArray, program).mkString(",")
}

def evaluatorTwo(input: List[String]): Long = {
    val (_, program) = parseInput(input)
    return generateA(program, program).min
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day17.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}