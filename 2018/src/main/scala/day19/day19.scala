package day19

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class State(regs: Vector[Int], ip: Int, ipReg: Int)

def executeInstruction(state: State, op: String, a: Int, b: Int, c: Int): State = {
    val regs = state.regs.updated(state.ipReg, state.ip)
    
    val newValue = op match {
        case "addr" => regs(a) + regs(b)
        case "addi" => regs(a) + b
        case "mulr" => regs(a) * regs(b)
        case "muli" => regs(a) * b
        case "banr" => regs(a) & regs(b)
        case "bani" => regs(a) & b
        case "borr" => regs(a) | regs(b)
        case "bori" => regs(a) | b
        case "setr" => regs(a)
        case "seti" => a
        case "gtir" => if (a > regs(b)) 1 else 0
        case "gtri" => if (regs(a) > b) 1 else 0
        case "gtrr" => if (regs(a) > regs(b)) 1 else 0
        case "eqir" => if (a == regs(b)) 1 else 0
        case "eqri" => if (regs(a) == b) 1 else 0
        case "eqrr" => if (regs(a) == regs(b)) 1 else 0
        case _ => throw new IllegalArgumentException(s"Unknown opcode: $op")
    }
    
    val newregs = regs.updated(c, newValue)
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