package day04

import scala.util.matching.Regex

def split(st: String) = new Iterator[String] {
    private var index = 0

    override def hasNext: Boolean = index < st.length

    override def next(): String = {
        val pattern = new Regex("[" + st(index) + "]+")
        val sequence = pattern.findFirstIn(st.substring(index)).getOrElse("")
        index += sequence.length
        sequence
    }
}

def ok(password: String, tripletsAllowed: Boolean): Boolean = {
    if (password.sliding(2).exists { it => it(0) > it(1) }) return false
    return split(password).exists { it => it.length >= 2 && (tripletsAllowed || it.length == 2) }
}

def solve(input: String, tripletsAllowed: Boolean): Int = {
    val Array(start, end) = input.split("-").map(_.toInt)
    return (start to end).count(n => ok(n.toString, tripletsAllowed))
}

def evaluatorOne(input: String): Int = solve(input, true)
def evaluatorTwo(input: String): Int = solve(input, false)

def main(): Unit = {
    val inputLine = "145852-616942"
    println(s"Part One: ${evaluatorOne(inputLine)}")
    println(s"Part Two: ${evaluatorTwo(inputLine)}")
}