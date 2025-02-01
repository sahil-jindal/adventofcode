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

def reverse(input: Array[Char], pos1: Int, pos2: Int) = {
    var x = pos1
    var y = pos2
    
    while (x < y) {
        swapPosition(input, x, y)
        x += 1
        y -= 1
    }
}

def rotateRight(input: Array[Char], pos: Int) = {
    val t = pos % input.length
    reverse(input, 0, input.length - 1);
    reverse(input, 0, t - 1);
    reverse(input, t, input.length - 1);
}

def executeInstructions(input: String, instructions: List[String]) = {
    val temp = input.toCharArray

    instructions.foreach { instruction =>
        instruction match {
            case operation1(a, b) => {
                swapPosition(temp, a.toInt, b.toInt)
            }
            case operation2(a, b) => {
                val chx = a(0)
                val chy = b(0)

                for i <- 0 until temp.length do {
                    if temp(i) == chx then temp(i) = chy
                    else if temp(i) == chy then temp(i) = chx
                }
            }
            case operation3(a) => {
                val t = a.toInt % temp.length
                reverse(temp, 0, t - 1);
                reverse(temp, t, temp.length - 1);
                reverse(temp, 0, temp.length - 1);
            }
            case operation4(a) => {
                rotateRight(temp, a.toInt)
            }
            case operation5(a) => {
                var i = temp.indexOf(a(0))
                val rotations = if i >= 4 then i + 2 else i + 1
                rotateRight(temp, rotations) 
            }
            case operation6(a, b) => {
                reverse(temp, a.toInt, b.toInt)
            }
            case operation7(a, b) => {
                val x = a.toInt
                val y = b.toInt

                var d = if x < y then 1 else -1
                var ch = temp(x);

                for (i <- x + d until y + d by d) {
                    temp(i - d) = temp(i)
                }

                temp(y) = ch;
            }
            case _ => println(s"Unrecognized instruction: $instruction")
        }
    }

    temp.mkString
}

def evaluatorOne(input: String, instructions: List[String]) = {
    executeInstructions(input, instructions)
}

def evaluatorTwo(base: String, dest: String, instructions: List[String]) = {
    base.permutations.find(it => executeInstructions(it, instructions) == dest).get
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