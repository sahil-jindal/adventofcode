package day16

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parseInput(input: String) = input.map(_.asDigit)

def pattern(digit: Int): Iterator[Int] = {
    val basePattern = List(0, 1, 0, -1)
    return Iterator.continually(basePattern.flatMap(List.fill(digit + 1))).flatten
}

def fft(digits: IndexedSeq[Int]): IndexedSeq[Int] = {
    return digits.indices.map(i => (digits zip pattern(i).drop(1)).map(_ * _).sum.abs % 10)
}

def evaluatorOne(digits: IndexedSeq[Int]): String = {
    return Iterator.iterate(digits)(fft).drop(100).next().take(8).mkString
}

def evaluatorTwo(digits: IndexedSeq[Int]): String = {
    val t = digits.take(7).mkString.toInt
    
    val (height, width) = (8, digits.length * 10000 - t)

    val bijMods = Array.ofDim[Int](width + 1)
    var bij = BigInt(1)

    for (j <- 1 to width) {
        bijMods(j) = (bij mod BigInt(10)).intValue()
        bij = (bij * BigInt(j + 99)) / BigInt(j)
    }

    val res = new StringBuilder

    for (i <- 1 to height) {
        var s = 0
        
        for (j <- i to width) {
            s += digits((t + j - 1) % digits.length) * bijMods(j - i + 1)
        }
        
        res.append((s % 10).toString)
    }

    return res.toString
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day16.txt") match {
        case Success(lines) => {
            val input = parseInput(lines.head)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}