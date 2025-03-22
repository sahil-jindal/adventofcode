package day16

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import java.math.BigInteger

def parseInput(input: String): Seq[Int] = input.map(_.asDigit)

def pattern(digit: Int): Iterator[Int] = {
    val basePattern = List(0, 1, 0, -1)
    return Iterator.continually(basePattern.flatMap(List.fill(digit + 1)(_))).flatten
}

def fft(digits: Seq[Int]): Seq[Int] = {
    return digits.indices.map(i => (digits zip pattern(i).drop(1)).map(_ * _).sum.abs % 10)
}

def evaluatorOne(input: String): String = {
    var digits = parseInput(input)
    for (_ <- 1 to 100) digits = fft(digits)
    return digits.take(8).mkString
}

def evaluatorTwo(input: String): String = {
    val xs = parseInput(input)
    val t = input.take(7).toInt
    
    val height = 8
    val width = input.length * 10000 - t

    val bijMods = Array.ofDim[Int](width + 1)
    var bij = BigInteger.ONE

    for (j <- 1 to width) {
        bijMods(j) = (bij.mod(BigInteger.TEN)).intValue()
        bij = bij.multiply(BigInteger.valueOf(j + 99)).divide(BigInteger.valueOf(j))
    }

    var res = ""

    for (i <- 1 to height) {
        var s = 0
        
        for (j <- i to width) {
            s += xs((t + j - 1) % input.length) * bijMods(j - i + 1)
        }
        
        res += (s % 10).toString
    }

    return res
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day16.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines.head)}")
            println(s"Part Two: ${evaluatorTwo(lines.head)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}