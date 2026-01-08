package day22

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Technique(a: BigInt, c: BigInt, m: BigInt) {
    infix def compose(other: Technique): Technique = {
        val m = this.m
        val a = (this.a * other.a) % m
        val c = (this.c * other.a + other.c) % m
        Technique(a, c, m)
    }

    def inverse(): Technique = {
        val m = this.m
        val a = this.a.modInverse(m)
        val c = m - (a * this.c) % m
        Technique(a, c, m)
    }

    def power(e: BigInt): Technique = {
        val m = this.m
        val a = this.a.modPow(e, m)
        val c = (((a - 1) * (this.a - 1).modInverse(m) % m) * this.c) % m
        Technique(a, c, m)
    }

    def shuffle(index: BigInt): BigInt = {
        (this.a * index + this.c) % this.m
    }
}

def deck(input: List[String], m: BigInt): Technique = {
    val techniques = input.collect {
        case l if l.contains("into new stack") => {
            Technique(m - 1, m - 1, m)
        }
        case l if l.startsWith("cut") => {
            val n = BigInt(l.split(" ").last)
            val c = (m - n % m) % m
            Technique(1, c, m)
        }
        case l if l.contains("increment") => {
            val n = BigInt(l.split(" ").last)
            val a = (m + n % m) % m;
            Technique(a, 0, m)
        }
    }

    return techniques.reduce(_ compose _)
}

def evaluatorOne(input: List[String]): BigInt = {
    return deck(input, 10007).shuffle(2019)
}

def evaluatorTwo(input: List[String]): BigInt = {
    return deck(input, 119315717514047L).inverse().power(101741582076661L).shuffle(2020)
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