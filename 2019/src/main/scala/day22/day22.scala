package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

def parse(input: List[String], m: BigInt, n: BigInt): (BigInt, BigInt) = {
    var a = BigInt(1)
    var b = BigInt(0)

    for (line <- input) {
        line match {
            case l if l.contains("into new stack") => {
                a = -a
                b = m - b - BigInt(1)
            }
            case l if l.startsWith("cut") => {
                val i = BigInt(line.split(" ").last)
                b = m + b - i
            }
            case l if l.contains("increment") => {
                val i = BigInt(line.split(" ").last)
                a *= i
                b *= i
            }
            case _ => throw new Exception("Invalid instruction")
        }
    }

    val resA = a.modPow(n, m)
    val resB = (b * (resA - BigInt(1)) * (a - BigInt(1)).modInverse(m)).mod(m)

    return (resA, resB)
}

def evaluatorOne(input: List[String]): BigInt = {
    val m = BigInt(10007)
    val iter = BigInt(1)
    val (a, b) = parse(input, m, iter)
    return (a * BigInt(2019) + b).mod(m)
}

def evaluatorTwo(input: List[String]): BigInt = {
    val m = BigInt(119315717514047L)
    val iter = BigInt(101741582076661L)
    val (a, b) = parse(input, m, iter)
    return (a.modInverse(m) * (BigInt(2020) - b)).mod(m)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day22.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}