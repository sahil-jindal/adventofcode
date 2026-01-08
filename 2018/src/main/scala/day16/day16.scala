package day16

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.control.Breaks._

// # Chronal Classification
//
// There are only 16 opcodes so we can use bitwise logic to efficiently perform the 
// set operations that uniquely determine the mapping of each opcode to instruction.
//
// First we create a bitmask for each instruction block in the first half of the 
// input with a `1` for each potential instruction. For example:
//
// ```none
//     Before: [3, 2, 1, 1]
//     9 2 1 2
//     After:  [3, 2, 2, 1]
//
//     Possible instructions: mulr, addi, seti
//     Binary Mask: 0000001000000110
// ```
//
// For part one the [`count_ones`] intrinsic computes the size of each set.
//
// For part two we need to determine the mapping of the unknown codes. First we reduce each
// unknown to a single set by taking the intersection of all examples. Then similar to
// solving simultaneous equation, we eliminate one unknown at a time, removing it from the other
// possibilities. This causes a domino effect, continuing until all unknowns are resolved.

case class Pair(unknown: Int, mask: Int)
case class Input(samples: List[Pair], prg: List[List[Int]])

def groupLines(input: List[String]): List[List[String]] = {
    return input.foldLeft(List(List.empty[String])) {
        case (acc, "") => acc :+ List.empty[String]
        case (acc, elem) => acc.init :+ (acc.last :+ elem)
    }.filter(_.nonEmpty)
}

def ints(input: String) = raw"(\d+)".r.findAllIn(input).map(_.toInt).toList

def parseInput(input: List[String]): Input = {
    val blocks = groupLines(input)

    val samples = blocks.init.map(group => {
        val List(before, instruction, after) = group.map(ints)
        val List(unknown, b, c, d) = instruction

        val mask = (for {
            opcode <- 0 until 16
            if cpu(opcode, b, c, before.toArray) == after(d)
        } yield 1 << opcode).sum

        Pair(unknown, mask)
    })

    val prg = blocks.last.map(ints)

    return Input(samples, prg)
}

def cpu(opcode: Int, b: Int, c: Int, regs: Array[Int]): Int = {
    return opcode match {
        case 0 => regs(b) + regs(c)
        case 1 => regs(b) + c
        case 2 => regs(b) * regs(c)
        case 3 => regs(b) * c
        case 4 => regs(b) & regs(c)
        case 5 => regs(b) & c
        case 6 => regs(b) | regs(c)
        case 7 => regs(b) | c
        case 8 => regs(b)
        case 9 => b
        case 10 => if (b > regs(c)) 1 else 0
        case 11 => if (regs(b) > c) 1 else 0
        case 12 => if (regs(b) > regs(c)) 1 else 0
        case 13 => if (b == regs(c)) 1 else 0
        case 14 => if (regs(b) == c) 1 else 0
        case 15 => if (regs(b) == regs(c)) 1 else 0
        case _ => throw new IllegalArgumentException()
    }
}

def evaluatorOne(input: Input): Int = {
    return input.samples.count(it => Integer.bitCount(it.mask) >= 3)
}

def evaluatorTwo(input: Input): Int = {
    // Take intersection of samples, reducing each unknown opcode to a single set of possibilities.
    val masks = Array.fill(16)(0xffff)

    input.samples.groupMapReduce(_.unknown)(_.mask)(_ & _).foreachEntry {
        case (unknown, mask) => masks(unknown) &= mask
    }

    // To uniquely determine the mapping, there must be at least 1 opcode during each iteration
    // that only has one possibility.
    val convert = Array.fill(16)(0)

    breakable {
        while (true) {
            val index = masks.indexWhere(n => Integer.bitCount(n) == 1)
            if (index == -1) break()

            val mask = masks(index)

            // This opcode has only 1 possible mapping, so remove possibility from other opcodes.
            for (j <- masks.indices) {
                masks(j) &= ~mask
            }

            // Add mapping.
            convert(index) = Integer.numberOfTrailingZeros(mask)
        }
    }
    
    // Run the program now that we know the mapping.
    val regs = Array.fill(4)(0)
    
    for (List(unknown, b, c, d) <- input.prg) { 
        regs(d) = cpu(convert(unknown), b, c, regs)
    }

    return regs(0)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day16.txt") match {
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