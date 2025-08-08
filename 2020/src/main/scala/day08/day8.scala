package day08

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Set

case class Stm(op: String, arg: Int)
case class Pair(acc: Int, terminated: Boolean)

def parseInput(input: List[String]) = input.map(line => {
    val Array(first, second) = line.split(" ")
    Stm(first, second.toInt)
})

def run(program: List[Stm]): Pair = {
    var (ip, acc) = (0, 0)
    val seen = Set.empty[Int]

    while (true) {
        if (ip >= program.length) return Pair(acc, true)
        if (seen.contains(ip)) return Pair(acc, false)
      
        seen += ip
        program(ip) match {
            case Stm("nop", _)  => ip += 1
            case Stm("acc", arg) => acc += arg; ip += 1
            case Stm("jmp", arg) => ip += arg
            case _ => ()
        }
    }

    throw new RuntimeException("Unreachable")
}

def patches(program: List[Stm]): List[List[Stm]] = {
    return program.zipWithIndex.collect { case (stm, idx) if stm.op != "acc" => idx }.map(lineToPatch => {
        val oldStm = program(lineToPatch)

        program.updated(lineToPatch, oldStm.op match {
            case "jmp" => oldStm.copy(op = "nop")
            case "nop" => oldStm.copy(op = "jmp")
            case _ => throw Exception()
        })
    })
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