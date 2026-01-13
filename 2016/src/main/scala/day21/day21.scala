package day21

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

sealed trait Operation { def applyString(str: String): String }

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

case class rotateBasedOnLetter(ch: Char) extends Operation {
    override def applyString(str: String) = {
        var i = str.indexOf(ch)
        val rotations = if i >= 4 then i + 2 else i + 1
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
        val temp = str.toCharArray
        
        var d = if x < y then 1 else -1
        var ch = temp(x)

        for (i <- x + d until y + d by d) {
            temp(i - d) = temp(i)
        }

        temp(y) = ch

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
    case operation5(a) => rotateBasedOnLetter(a.head)
    case operation6(a, b) => reversePositions(a.toInt, b.toInt)
    case operation7(a, b) => moveCharacters(a.toInt, b.toInt)
}

def executeInstructions(input: String, instructions: List[Operation]): String = {
    return instructions.foldLeft(input) { case (curr, instr) => instr.applyString(curr) }
}

def evaluatorOne(instructions: List[Operation]): String = {
    return executeInstructions("abcdefgh", instructions)
}

def evaluatorTwo(instructions: List[Operation]): String = {
    return "abcdefgh".permutations.find(executeInstructions(_, instructions) == "fbgdceah").get
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