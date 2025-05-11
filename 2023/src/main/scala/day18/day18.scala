package day18

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Group(dir: Char, amount: Int, color: String)

case class Complex(real: Long, imag: Long) {
    def *(num: Int) = Complex(real * num, imag * num)
    def +(that: Complex) = Complex(real + that.real, imag + that.imag)
    def magnitude = Math.sqrt(real.toDouble * real.toDouble + imag.toDouble * imag.toDouble)
}

def parseInput(input: List[String]) = input.map(line => {
    val parts = line.split(" ")
    Group(parts(0).head, parts(1).toInt, parts(2).substring(2, 8))
})

def stepsOne(input: List[Group]) = input.map(group => {
    val dir = group.dir match {
        case 'R' => Complex(1, 0)
        case 'U' => Complex(0, -1)
        case 'L' => Complex(-1, 0)
        case 'D' => Complex(0, 1)
    }

    dir * group.amount
})

def stepsTwo(input: List[Group]) = input.map(group => {
    val dir = group.color.last match {
        case '0' => Complex(1, 0)
        case '1' => Complex(0, 1)
        case '2' => Complex(-1, 0)
        case '3' => Complex(0, -1)
    }

    dir * Integer.parseInt(group.color.init, 16) 
})

def getVertices(steps: List[Complex]): List[Complex] = {
    return steps.scanLeft(Complex(0, 0))(_ + _).tail
}

// We are using a combination of the shoelace formula with Pick's theorem
def area(steps: List[Complex]): Long = {
    val vertices = getVertices(steps)

    // Shoelace formula https://en.wikipedia.org/wiki/Shoelace_formula
    val shiftedVertices = vertices.tail :+ vertices.head

    val shoelaces = (vertices zip shiftedVertices).map { 
        case (p1, p2) => p1.real * p2.imag - p1.imag * p2.real
    }

    val area = shoelaces.sum.abs / 2

    // Pick's theorem  https://en.wikipedia.org/wiki/Pick%27s_theorem
    val boundary = steps.map(_.magnitude).sum.toLong
    val interior = area - (boundary / 2) + 1

    // integer area
    return boundary + interior
}

def evaluatorOne(input: List[Group]): Long = area(stepsOne(input))
def evaluatorTwo(input: List[Group]): Long = area(stepsTwo(input))

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day18.txt") match {
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