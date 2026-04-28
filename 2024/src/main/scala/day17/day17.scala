package day17

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.ListBuffer

enum Opcode { case Adv, Bxl, Bst, Jnz, Bxc, Out, Bdv, Cdv }

case class Input(state: Vector[Long], program: Vector[Int])

def parseNums(st: String) = raw"(\d+)".r.findAllIn(st).map(_.toInt).toVector

def parseInput(input: List[String]): Input = {
    val regA = input(0).stripPrefix("Register A: ").toLong
    val regB = input(1).stripPrefix("Register B: ").toLong
    val regC = input(2).stripPrefix("Register C: ").toLong
    val program = parseNums(input.last)
    return Input(Vector(regA, regB, regC), program)
}

def run(currState: Vector[Long], program: Vector[Int]): Vector[Int] = {
    var ip = 0
    val state = currState.toArray
    val res = ListBuffer.empty[Int]
    
    def combo(op: Int) = if op < 4L then op.toLong else state(op - 4)

    while (ip < program.size) {
        (Opcode.fromOrdinal(program(ip)), program(ip + 1)) match {
            case (Opcode.Bxl, op) => state(1) ^= op
            case (Opcode.Bxc, op) => state(1) ^= state(2)
            case (Opcode.Adv, op) => state(0) >>= combo(op)
            case (Opcode.Bst, op) => state(1) = combo(op) % 8
            case (Opcode.Out, op) => res += (combo(op) % 8).toInt
            case (Opcode.Bdv, op) => state(1) = state(0) >> combo(op)
            case (Opcode.Cdv, op) => state(2) = state(0) >> combo(op)
            case (Opcode.Jnz, op) => ip = if state(0) == 0 then ip else op - 2
        }

        ip += 2
    }

    return res.toVector
}

// Determines register A for the given output. The search works recursively and in 
// reverse order, starting from the last number to be printed and ending with the first.
def generateA(program: Vector[Int], output: Vector[Int]): List[Long] = {
    if (output.isEmpty) return List(0L)

    return generateA(program, output.tail)
        .flatMap(ah => (0 to 7).map(al => ah * 8 + al))
        .filter(a => run(Vector(a, 0, 0), program).sameElements(output))
}

def evaluatorOne(input: Input): String = {
    val Input(state, program) = input
    return run(state, program).mkString(",")
}

def evaluatorTwo(input: Input): Long = {
    return generateA(input.program, input.program).min
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day17.txt") match {
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