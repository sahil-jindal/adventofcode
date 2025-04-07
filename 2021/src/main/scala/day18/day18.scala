package day18

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.util.boundary, boundary.break;

sealed trait TokenKind
case object Open extends TokenKind
case object Close extends TokenKind
case object Digit extends TokenKind

case class Token(kind: TokenKind, value: Int = 0)

class Number(private var inner: List[Token]) {
    def length: Int = inner.length
    def toList: List[Token] = inner
    def apply(i: Int): Token = inner(i)
    def isEmpty: Boolean = inner.isEmpty
    def add(token: Token): Unit = inner :+= token
    def addAll(tokens: List[Token]): Unit = inner ++= tokens
    def update(i: Int, token: Token): Unit = inner = inner.updated(i, token)
    def removeRange(i: Int, n: Int): Unit = inner = inner.take(i) ++ inner.drop(i + n)
    def insert(i: Int, token: Token): Unit = inner = inner.take(i) ++ List(token) ++ inner.drop(i)
    def insertAll(i: Int, number: Number): Unit = inner = inner.take(i) ++ number.toList ++ inner.drop(i)
}

object Number {
    def apply(tokens: List[Token] = Nil): Number = new Number(tokens)
    def digit(value: Int): Number = Number(List(Token(Digit, value)))
    def pair(a: Number, b: Number): Number = Number(Token(Open) :: ((a.toList ++ b.toList) :+ Token(Close)))
}

 def parseNumber(input: String): Number = {
    val tokens = input.foldLeft((List.empty[Token], "")) {
        case ((list, acc), ch) if ch.isDigit => (list, acc + ch)
        case ((list, acc), ch) => {
            val withDigit = if (acc.nonEmpty) list :+ Token(Digit, acc.toInt) else list
            ch match {
                case '[' => (withDigit :+ Token(Open), "")
                case ']' => (withDigit :+ Token(Close), "")
                case _   => (withDigit, "")
            }
        }
    } match {
        case (list, acc) if acc.nonEmpty => list :+ Token(Digit, acc.toInt)
        case (list, _)                   => list
    }

    Number(tokens)
}

def explode(number: Number): Boolean = {
    var depth = 0

    boundary {
        for (i <- 0 until number.length) {
            if (number(i).kind == Open) {
                depth += 1
                if (depth == 5) {
                    val leftValue = number(i + 1).value
                    val rightValue = number(i + 2).value

                    // Left digit
                    val leftFound = (i - 1 to 0 by -1).find(j => number(j).kind == Digit)
                    
                    if (leftFound.isDefined) {
                        val j = leftFound.get
                        number(j) = number(j).copy(value = number(j).value + leftValue)
                    }

                    // Right digit
                    val rightFound = ((i + 3) until number.length).find(j => number(j).kind == Digit)
                    
                    if (rightFound.isDefined) {
                        val j = rightFound.get
                        number(j) = number(j).copy(value = number(j).value + rightValue)
                    }

                    number.removeRange(i, 4)
                    number.insert(i, Token(Digit, 0))
                    break(true)
                }
            } else if (number(i).kind == Close) { depth -= 1 }
        }

        false
    }
}

 def split(number: Number): Boolean = {
    boundary {
        for (i <- 0 until number.length) {
            if (number(i).kind == Digit && number(i).value >= 10) {
                val v = number(i).value
                val left = Number.digit(v / 2)
                val right = Number.digit((v + 1) / 2)
                number.removeRange(i, 1)
                number.insertAll(i, Number.pair(left, right))
                break(true)
            }
        }

        false
    }
}

def reduce(number: Number): Number = {
    while (explode(number) || split(number)) {}
    return number
}

def sum(a: Number, b: Number): Number = reduce(Number.pair(a, b))

def magnitude(number: Number): Long = {
    var i = 0

    def compute(): Long = number(i) match {
        case Token(Digit, v) => {
            i += 1
            v
        }
        case Token(Open, _) => {
            i += 1
            val left = compute()
            val right = compute()
            i += 1 // skip Close
            3 * left + 2 * right
        }
        case _ => throw new Exception("Unexpected token")
    }

    compute()
}

def evaluatorOne(input: List[String]): Long = {
    val numbers = input.map(parseNumber)
    
    val res = numbers.reduceLeft { (acc, number) =>
        if (acc.isEmpty) number else sum(acc, number)
    }

    return magnitude(res)
}

def evaluatorTwo(input: List[String]): Long = {
    val numbers = input.map(parseNumber)

    return (for {
        i <- numbers.indices
        j <- numbers.indices
        if i != j
    } yield magnitude(sum(numbers(i), numbers(j)))).max
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day18.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}