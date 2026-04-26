package day21

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

//! # Scrambled Letters and Hash
//!
//! The forward transformations are straightforward. The trickiest reverse transformation is the
//! rotation based on the index of the letter. First we build a lookup table of how many places to
//! rotate right based on the letter index. This is +1 for positions 0-3 and +2 for positions 4-7.
//!
//! Then we invert this by mapping the transformed index to the rotation. For example position 3 is
//! rotated right by 4 places, ending up at position 7, so the inverse lookup table to rotate left
//! stores 4 at index 7.

val ROTATE_LETTER_RIGHT = Vector(1, 2, 3, 4, 6, 7, 0, 1)
val ROTATE_LETTER_LEFT = Vector(1, 1, 6, 2, 7, 3, 0, 4)

trait Operation { def applyString(str: String): String }

case class swapPosition(a: Int, b: Int) extends Operation {
    override def applyString(str: String) = {
        val input = str.toCharArray
        val temp = input(a)
        input(a) = input(b)
        input(b) = temp
        return input.mkString
    }
}

case class swapLetter(a: Char, b: Char) extends Operation {
    override def applyString(str: String) = {
        return str.collect {
            case ch if ch == a => b
            case ch if ch == b => a
            case other => other
        }
    }
}

case class rotateLeft(num: Int) extends Operation {
    override def applyString(str: String) = {
        val t = num % str.length
        return str.drop(t) ++ str.take(t)
    }
}

case class rotateRight(num: Int) extends Operation {
    override def applyString(str: String) = {
        val t = num % str.length
        return str.takeRight(t) ++ str.dropRight(t)
    }
}

case class rotateLetterLeft(ch: Char) extends Operation {
    override def applyString(str: String) = {
        val first = str.indexOf(ch)
        val rotations = ROTATE_LETTER_LEFT(first) % str.length
        return rotateLeft(rotations).applyString(str)
    }
}

case class rotateLetterRight(ch: Char) extends Operation {
    override def applyString(str: String) = {
        val first = str.indexOf(ch)
        val rotations = ROTATE_LETTER_RIGHT(first) % str.length
        return rotateRight(rotations).applyString(str)
    }
}

case class reversePositions(a: Int, b: Int) extends Operation {
    override def applyString(str: String) = {
        val temp = str.toCharArray
        val reversed = str.slice(a, b + 1).reverse
        reversed.zipWithIndex.foreach { case (ch, i) => temp(a + i) = ch }
        return temp.mkString
    }
}

case class moveCharacters(x: Int, y: Int) extends Operation {
    override def applyString(str: String) = {
        val temp = str.toBuffer
        val letter = temp.remove(x)
        temp.insert(y, letter)
        return temp.mkString
    }
}

val operation1 = raw"swap position (\d+) with position (\d+)".r
val operation2 = raw"swap letter (\w) with letter (\w)".r
val operation3 = raw"rotate left (\d+) step[s]?".r
val operation4 = raw"rotate right (\d+) step[s]?".r
val operation5 = raw"rotate based on position of letter (\w)".r
val operation6 = raw"reverse positions (\d+) through (\d+)".r
val operation7 = raw"move position (\d+) to position (\d+)".r

def parseInput(input: List[String]) = input.collect {
    case operation1(a, b) => swapPosition(a.toInt, b.toInt)
    case operation2(a, b) => swapLetter(a.head, b.head)
    case operation3(a) => rotateLeft(a.toInt)
    case operation4(a) => rotateRight(a.toInt)
    case operation5(a) => rotateLetterRight(a.head)
    case operation6(a, b) => reversePositions(a.toInt, b.toInt)
    case operation7(a, b) => moveCharacters(a.toInt, b.toInt)
}

def inverse(operation: Operation) = operation match {
    case rotateLeft(num) => rotateRight(num)
    case rotateRight(num) => rotateLeft(num)
    case rotateLetterLeft(ch) => rotateLetterRight(ch)
    case rotateLetterRight(ch) => rotateLetterLeft(ch)
    case moveCharacters(x, y) => moveCharacters(y, x)
    case other => other
}

def executeInstructions(input: String, instructions: List[Operation]): String = {
    return instructions.foldLeft(input) { case (curr, instr) => instr.applyString(curr) }
}

def evaluatorOne(instructions: List[Operation]): String = {
    return executeInstructions("abcdefgh", instructions)
}

def evaluatorTwo(instructions: List[Operation]): String = {
    return executeInstructions("fbgdceah", instructions.reverse.map(inverse))
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day21.txt") match {
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