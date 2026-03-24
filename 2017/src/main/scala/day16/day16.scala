package day16

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

extension (arr: Array[Int]) {
    def swap(a: Int, b: Int): Unit = {
        val temp = arr(a)
        arr(a) = arr(b)
        arr(b) = temp
    }
}

sealed trait Move
case class Spin(size: Int) extends Move
case class Exchange(a: Int, b: Int) extends Move
case class Partner(a: Char, b: Char) extends Move

case class Dance(position: Vector[Int], exchange: Vector[Int]) {
    def this() = this((0 to 15).toVector, (0 to 15).toVector)
    def apply() = position.map(i => ('a' + exchange(i)).toChar).mkString
    
    def compose(other: Dance) = Dance(
        position = this.position.map(i => other.position(i)),
        exchange = this.exchange.map(i => other.exchange(i))
    )
}

def parseInput(input: String) = input.split(',').collect {
    case s"s$num" => Spin(num.toInt)
    case s"x$a/$b" => Exchange(a.toInt, b.toInt)
    case s"p$a/$b" => Partner(a.head, b.head)
}.toList

def makeDance(input: List[Move]): Dance = {
    val Array(position, exchange, lookup) = Array.fill(3)((0 to 15).toArray)
    var offset = 0

    for (op <- input) {
        op match {
            // Increasing the offset has the same effect as rotating elements to the right.
            case Spin(size) => {
                offset += 16 - size
            }
            // Swap two elements taking into account the offset when calculating indices.
            case Exchange(a, b) => {
                position.swap((a + offset) % 16, (b + offset) % 16)
            }
             // First lookup the index of each letter, then swap the mapping.
            case Partner(a, b) => {
                val (first, second) = (a - 'a', b - 'a')
                lookup.swap(first, second)
                exchange.swap(lookup(first), lookup(second))
            }
        }
    }

    // Rotate the array once to apply all spins.
    val temp = offset % 16
    val (a, b) = position.splitAt(temp)

    return Dance((b ++ a).toVector, exchange.toVector)
}

def evaluatorOne(input: Dance) = input.apply()

def evaluatorTwo(input: Dance): String = {
    var dance = input

    // 1 billion is 0b00111011_10011010_11001010_00000000: 30 bits, with 13 set.  Typical
    // exponentiation by squaring would be 30 doubles and 13 additions, or 43 calls to
    // compose; but since one billion is a power of ten, we can do better by 9 cycles of
    // reaching each next power of ten by two doubles, one addition, and one more double
    // per cycle, for a total of only 36 calls to compose.
    for (_ <- 0 until 9) {
        val dance2 = dance.compose(dance)
        val dance5 = dance2.compose(dance2).compose(dance)
        dance = dance5.compose(dance5)
    }

    return dance.apply()
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day16.txt") match {
        case Success(lines) => {
            val input = makeDance(parseInput(lines.head))
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}