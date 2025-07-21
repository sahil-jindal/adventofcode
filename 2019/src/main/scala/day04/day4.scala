package day04

import scala.collection.immutable.Range.Inclusive

def split(str: String) = new Iterator[Int] {
    private var idx = 0

    override def hasNext = idx < str.length

    override def next(): Int = {
        val ch = str(idx)
        val len = str.substring(idx).takeWhile(_ == ch).length
        idx += len
        len
    }
}

def sortedString(str: String) = (str.init zip str.tail).forall { case (a, b) => a <= b }

def partOneOkay(password: String) = split(password).exists(_ >= 2)
def partTwoOkay(password: String) = split(password).exists(_ == 2)

def solver(numRange: Inclusive, okayString: String => Boolean): Int = {
    return numRange.map(_.toString).filter(sortedString).count(okayString)
}

def evaluatorOne(input: Inclusive): Int = solver(input, partOneOkay)
def evaluatorTwo(input: Inclusive): Int = solver(input, partTwoOkay)

def main(): Unit = {
    val inputLine = "145852-616942"
    val Array(start, end) = inputLine.split("-").map(_.toInt)
    println(s"Part One: ${evaluatorOne(start to end)}")
    println(s"Part Two: ${evaluatorTwo(start to end)}")
}