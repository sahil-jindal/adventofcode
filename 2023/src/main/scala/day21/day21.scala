package day21

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Complex(real: Int, imag: Int) {
    def +(that: Complex) = Complex(real + that.real, imag + that.imag)
}

def parseInput(input: List[String]): Set[Complex] = {
    return (for {
        (line, y) <- input.zipWithIndex
        (ch, x) <- line.zipWithIndex
        if ch != '#'
    } yield Complex(x, y)).toSet
}

// the double % takes care of negative numbers
def mod(n: Int, m: Int) = ((n % m) + m) % m

def step(map: Set[Complex], positions: Set[Complex]): Set[Complex] = {
    val dirs = Seq(Complex(1, 0), Complex(-1, 0), Complex(0, 1), Complex(0, -1))

    return (for {
        pos <- positions
        dir <- dirs
        posT = pos + dir
        tileCol = mod(posT.real, 131)
        tileRow = mod(posT.imag, 131)
        if map.contains(Complex(tileCol, tileRow))
    } yield posT).toSet
}

def getSteps(map: Set[Complex]): LazyList[Int] = {
    def loop(positions: Set[Complex]): LazyList[Int] = {
        positions.size #:: loop(step(map, positions))
    }
    
    loop(Set(Complex(65, 65)))
}

def evaluatorOne(input: Set[Complex]): Int = getSteps(input)(64)

def evaluatorTwo(input: Set[Complex]): Long = {
    // Exploiting some nice properties of the input it reduces to quadratic 
    // interpolation over 3 points: k * 131 + 65 for k = 0, 1, 2
    // I used the Newton method.
    
    val steps = getSteps(input).take(328).toVector

    val (x0, y0) = (65d, steps(65).toDouble)
    val (x1, y1) = (196d, steps(196).toDouble)
    val (x2, y2) = (327d, steps(327).toDouble)

    val y01 = (y1 - y0) / (x1 - x0)
    val y12 = (y2 - y1) / (x2 - x1)
    val y012 = (y12 - y01) / (x2 - x0)

    val n = 26501365

    BigDecimal(y0 + y01 * (n - x0) + y012 * (n - x0) * (n - x1)).setScale(0, BigDecimal.RoundingMode.HALF_UP).toLong
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

@main
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