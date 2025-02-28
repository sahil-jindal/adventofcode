package day19

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class State(regs: Vector[Int], ip: Int, ipReg: Int)

def executeInstruction(state: State, op: String, a: Int, b: Int, c: Int): State = {
    val regs = state.regs.updated(state.ipReg, state.ip)
    
    val newregs = op match {
        case "addr" => regs.updated(c, regs(a) + regs(b))
        case "addi" => regs.updated(c, regs(a) + b)
        case "mulr" => regs.updated(c, regs(a) * regs(b))
        case "muli" => regs.updated(c, regs(a) * b)
        case "banr" => regs.updated(c, regs(a) & regs(b))
        case "bani" => regs.updated(c, regs(a) & b)
        case "borr" => regs.updated(c, regs(a) | regs(b))
        case "bori" => regs.updated(c, regs(a) | b)
        case "setr" => regs.updated(c, regs(a))
        case "seti" => regs.updated(c, a)
        case "gtir" => regs.updated(c, if (a > regs(b)) 1 else 0)
        case "gtri" => regs.updated(c, if (regs(a) > b) 1 else 0)
        case "gtrr" => regs.updated(c, if (regs(a) > regs(b)) 1 else 0)
        case "eqir" => regs.updated(c, if (a == regs(b)) 1 else 0)
        case "eqri" => regs.updated(c, if (regs(a) == b) 1 else 0)
        case "eqrr" => regs.updated(c, if (regs(a) == regs(b)) 1 else 0)
        case _ => throw new IllegalArgumentException(s"Unknown opcode: $op")
    }
    
    val newIp = newregs(state.ipReg) + 1

    return State(newregs, newIp, state.ipReg)
}

def executeProgram(state: State, instructions: List[(String, Int, Int, Int)]): State = {
    var currentState = state
    
    while (currentState.ip >= 0 && currentState.ip < instructions.length) {
        val (op, a, b, c) = instructions(currentState.ip)
        currentState = executeInstruction(currentState, op, a, b, c)
    }
    
    return currentState
}

def evaluatorOne(lines: List[String]): Int = {
    var ipReg = lines.head.substring("#ip ".length).toInt
    
    var instructions = lines.tail.map(it => {
        val parts = it.split(" ")
        (parts(0), parts(1).toInt, parts(2).toInt, parts(3).toInt)
    })

    val initialState = State(Vector.fill(6)(0), 0, ipReg)

    return executeProgram(initialState, instructions).regs(0)
}

def evaluatorTwo(): Int = {
    val n = 10551345
    return (1 to n).filter(it => n % it == 0).sum
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day19.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo()}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}