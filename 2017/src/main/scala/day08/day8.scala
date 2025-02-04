package day08

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

val comparators = Map[String, (Int, Int) => Boolean](
    (">=", (x, y) => x >= y),
    ("<=", (x, y) => x <= y),
    ("==", (x, y) => x == y),
    ("!=", (x, y) => x != y),
    (">",  (x, y) => x > y),
    ("<",  (x, y) => x < y),
)

val operations = Map[String, (Int, Int) => Int](
    ("inc", (x, y) => x + y),
    ("dec", (x, y) => x - y)
)

def solve(input: List[String]): (Int, Int) = {
    val prg = input.map(_.split(' '))
    var regs = Map[String, Int]().withDefaultValue(0)
    var maxEver = Int.MinValue

    for instruction <- prg do {
        val op = operations(instruction(1))
        val comp = comparators(instruction(5))

        val reg1 = instruction(0)
        val num1 = instruction(2).toInt

        val reg2 = regs(instruction(4))
        val num2 = instruction(6).toInt

        if(comp(reg2, num2)) then {
            regs(reg1) = op(regs(reg1), num1)
            maxEver = Math.max(maxEver, regs(reg1))
        }   
    }
    
    (regs.values.max, maxEver)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

@main
def hello(): Unit =
    readLinesFromFile("day08.txt") match
        case Success(lines) => {
            val (partOne, partTwo) = solve(lines)
            println(s"Part One: $partOne")
            println(s"Part Two: $partTwo")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }