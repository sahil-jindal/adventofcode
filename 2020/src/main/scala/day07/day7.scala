package day07

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source

case class Bag(amount: Int, next: Int)
case class Haversack(shinyGold: Int, bags: Vector[IndexedSeq[Bag]])

val bagPattern = raw"(^[a-z]+) ([a-z]+) bag".r
val childrenPattern = raw"(\d+) ([a-z]+) ([a-z]+) bag".r

val adjectives = Vector(
    "bright", "clear", "dark", "dim", "dotted", "drab", "dull", "faded", "light",
    "mirrored", "muted", "pale", "plaid", "posh", "shiny", "striped", "vibrant", "wavy"
)

val colors = Vector(
    "aqua", "beige", "blue", "black", "brown", "bronze", "chartreuse", "coral", "crimson", "cyan", "fuchsia",
    "gold", "green", "gray", "indigo", "lavender", "lime", "maroon", "magenta", "olive", "orange", "plum",
    "purple", "red", "salmon", "silver", "tan", "teal", "tomato", "turquoise", "violet", "white", "yellow"
)

def perfectMinimalHash(first: String, second: String): Int = {
    val firstIndex = adjectives.indexOf(first)
    val secondIndex = colors.indexOf(second)
    return secondIndex * 18 + firstIndex
}

def parseInput(input: List[String]): Haversack = {
    val bags = Array.ofDim[IndexedSeq[Bag]](594)

    for (line <- input) {
        val m = bagPattern.findFirstMatchIn(line).get
        val outer = perfectMinimalHash(m.group(1), m.group(2))
    
        bags(outer) = childrenPattern.findAllMatchIn(line).map(m => { 
            val List(amount, first, second) = m.subgroups
            Bag(amount.toInt, perfectMinimalHash(first, second))
        }).toIndexedSeq
    }

    val shinyGold = perfectMinimalHash("shiny", "gold")
    return Haversack(shinyGold, bags.toVector)
}

def evaluatorOne(input: Haversack): Int = {
    val Haversack(shinyGold, bags) = input

    val cache = Array.fill[Option[Boolean]](594)(None)
    cache(shinyGold) = Some(true)

    def helper(key: Int): Boolean = {
        if (cache(key).isDefined) { return cache(key).get }
        val value = bags(key).exists(it => helper(it.next))
        cache(key) = Some(value)
        return value
    }

    return bags.indices.count(helper) - 1
}

def evaluatorTwo(input: Haversack): Int = {
    val Haversack(shinyGold, bags) = input
    val cache = Array.fill[Option[Int]](594)(None)

    def helper(key: Int): Int = {
        if (cache(key).isDefined) { return cache(key).get }
        val value = 1 + bags(key).map(it => it.amount * helper(it.next)).sum
        cache(key) = Some(value)
        return value
    }

    return helper(shinyGold) - 1
}

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day07.txt") match {
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