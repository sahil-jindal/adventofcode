package day08

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.{Map => MutableMap}

case class Instruction(reg1: String, op: Int => Int, reg2: String, comp: Int => Boolean)

val comparators = Map[String, Int => Int => Boolean](
    (">=", y => x => x >= y), ("<=", y => x => x <= y),
    ("==", y => x => x == y), ("!=", y => x => x != y),
    (">",  y => x => x > y), ("<",  y => x => x < y),
)

val operations = Map[String, Int => Int => Int](
    ("inc", y => x => x + y), ("dec", y => x => x - y)
)

def parseInput(input: List[String]) = input.map(line => {
    val Array(a, b, c, _, e, f, g) = line.split(' ')
    Instruction(a, operations(b)(c.toInt), e, comparators(f)(g.toInt))
})

def solve(prg: List[Instruction]): (Int, Int) = {
    val regs = MutableMap.empty[String, Int].withDefaultValue(0)
    var maxEver = Int.MinValue

    for (Instruction(a, op, b, comp) <- prg) {
        if (comp(regs(b))) {
            regs(a) = op(regs(a))
            maxEver = Math.max(maxEver, regs(a))
        }   
    }
    
    return (regs.values.max, maxEver)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day08.txt") match {
        case Success(lines) => {
            val (partOne, partTwo) = solve(parseInput(lines))
            println(s"Part One: $partOne")
            println(s"Part Two: $partTwo")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}