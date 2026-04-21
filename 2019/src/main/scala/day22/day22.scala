package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Technique[M <: Long](a: BigInt, c: BigInt)(using v: ValueOf[M]) {
    private val m = BigInt(v.value)

    infix def compose(other: Technique[M]): Technique[M] = {
        val a = (this.a * other.a) % m
        val c = (this.c * other.a + other.c) % m
        Technique[M](a, c)
    }

    def inverse(): Technique[M] = {
        val a = this.a.modInverse(m)
        val c = m - (a * this.c) % m
        Technique[M](a, c)
    }

    def power(e: BigInt): Technique[M] = {
        val a = this.a.modPow(e, m)
        val c = (((a - 1) * (this.a - 1).modInverse(m) % m) * this.c) % m
        Technique[M](a, c)
    }

    def shuffle(index: BigInt): BigInt = {
        (this.a * index + this.c) % m
    }
}

def deck[M <: Long](input: List[String])(using v: ValueOf[M]): Technique[M] = {
    val m = BigInt(v.value)

    val techniques = input.collect {
        case s"cut $num" => {
            val n = BigInt(num)
            val c = (m - n % m) % m
            Technique[M](1, c)
        }
        case "deal into new stack" => {
            Technique[M](m - 1, m - 1)
        }
        case s"deal with increment $num" => {
            val n = BigInt(num)
            val a = (m + n % m) % m;
            Technique[M](a, 0)
        }
    }

    return techniques.reduce(_ compose _)
}

def evaluatorOne(input: List[String]): BigInt = {
    return deck[10007L](input).shuffle(2019)
}

def evaluatorTwo(input: List[String]): BigInt = {
    return deck[119315717514047L](input).inverse().power(101741582076661L).shuffle(2020)
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