package day21

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

val operation1 = "swap position (\\d+) with position (\\d+)".r
val operation2 = "swap letter (\\w) with letter (\\w)".r
val operation3 = "rotate left (\\d+) step[s]?".r
val operation4 = "rotate right (\\d+) step[s]?".r
val operation5 = "rotate based on position of letter (\\w)".r
val operation6 = "reverse positions (\\d+) through (\\d+)".r
val operation7 = "move position (\\d+) to position (\\d+)".r

def swapPosition(input: Array[Char], pos1: Int, pos2: Int) = {
    val temp = input(pos1)
    input(pos1) = input(pos2)
    input(pos2) = temp
}

def Reverse(input: Array[Char], pos1: Int, pos2: Int) = {
    var x = pos1
    var y = pos2
    
    while (x < y) {
        swapPosition(input, x, y)
        x += 1
        y -= 1
    }
}

def RotateRight(input: Array[Char], pos: Int) = {
    val t = pos % input.length
    Reverse(input, 0, input.length - 1);
    Reverse(input, 0, t - 1);
    Reverse(input, t, input.length - 1);
}

def executeInstructions(input: Array[Char], instructions: List[String]) = {
    instructions.foreach { instruction =>
        instruction match {
            case operation1(a, b) => {
                swapPosition(input, a.toInt, b.toInt)
            }
            case operation2(a, b) => {
                val chx = a(0)
                val chy = b(0)

                for i <- 0 until input.length do {
                    if input(i) == chx then input(i) = chy
                    else if input(i) == chy then input(i) = chx
                }
            }
            case operation3(a) => {
                val t = a.toInt % input.length
                Reverse(input, 0, t - 1);
                Reverse(input, t, input.length - 1);
                Reverse(input, 0, input.length - 1);
            }
            case operation4(a) => {
                RotateRight(input, a.toInt)
            }
            case operation5(a) => {
                var i = input.indexOf(a(0))
                val rotations = if i >= 4 then i + 2 else i + 1
                RotateRight(input, rotations) 
            }
            case operation6(a, b) => {
                Reverse(input, a.toInt, b.toInt)
            }
            case operation7(a, b) => {
                val x = a.toInt
                val y = b.toInt

                var d = if x < y then 1 else -1
                var ch = input(x);

                for (i <- x + d until y + d by d) {
                    input(i - d) = input(i)
                }

                input(y) = ch;
            }
            case _ => println(s"Unrecognized instruction: $instruction")
        }
    }
}

def evaluatorOne(input: String, instructions: List[String]) = {
    val temp = input.toCharArray
    executeInstructions(temp, instructions)
    temp.mkString
}

def evaluatorTwo(base: String, dest: String, instructions: List[String]) = {
    base.permutations.find(it => {
        val temp = it.toCharArray 
        executeInstructions(temp, instructions)
        temp.mkString == dest
    }).get
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

@main
def hello(): Unit =
    readLinesFromFile("day21.txt") match
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne("abcdefgh", lines)}")
            println(s"Part Two: ${evaluatorTwo("abcdefgh", "fbgdceah", lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }