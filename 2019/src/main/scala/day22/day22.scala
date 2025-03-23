package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import java.math.BigInteger

def mod(a: BigInteger, m: BigInteger): BigInteger = (a.mod(m).add(m)).mod(m)
def modInv(a: BigInteger, m: BigInteger): BigInteger = a.modPow(m.subtract(BigInteger.TWO), m)

def parse(input: List[String], m: BigInteger, n: BigInteger): (BigInteger, BigInteger) = {
    var a = BigInteger.ONE
    var b = BigInteger.ZERO

    for (line <- input) {
        line match {
            case l if l.contains("into new stack") => {
                a = a.negate()
                b = m.subtract(b).subtract(BigInteger.ONE)
            }
            case l if l.startsWith("cut") => {
                val i = new BigInteger(line.split(" ").last)
                b = mod(m.add(b).subtract(i), m)
            }
            case l if l.contains("increment") => {
                val i = new BigInteger(line.split(" ").last)
                a = mod(a.multiply(i), m)
                b = mod(b.multiply(i), m)
            }
            case _ => throw new Exception("Invalid instruction")
        }
    }

    val resA = a.modPow(n, m)
    val resB = mod(b.multiply(resA.subtract(BigInteger.ONE)).multiply(modInv(a.subtract(BigInteger.ONE), m)), m)

    return (resA, resB)
}

def evaluatorOne(input: List[String]): BigInteger = {
    val m = BigInteger.valueOf(10007)
    val iter = BigInteger.ONE
    val (a, b) = parse(input, m, iter)
    return mod(a.multiply(BigInteger.valueOf(2019)).add(b), m)
}

def evaluatorTwo(input: List[String]): BigInteger = {
    val m = BigInteger.valueOf(119315717514047L)
    val iter = BigInteger.valueOf(101741582076661L)
    val (a, b) = parse(input, m, iter)
    return mod(modInv(a, m).multiply(BigInteger.valueOf(2020).subtract(b)), m)
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

@main
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