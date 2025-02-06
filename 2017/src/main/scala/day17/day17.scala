package day17

import scala.collection.mutable.ListBuffer

def evaluatorOne(input: String): Int = {
    val step = input.toInt
    val nums = ListBuffer(0)
    var pos = 0

    for (i <- 1 until 2018) {
        pos = (pos + step) % nums.length + 1
        nums.insert(pos, i)
    }

    nums((pos + 1) % nums.length)
}

def evaluatorTwo(input: String): Int = {
    val step = input.toInt
    var pos = 0
    var numsCount = 1
    var res = 0

    for (i <- 1 until 50000001) {
        pos = (pos + step) % numsCount + 1
        if (pos == 1) res = i
        numsCount += 1
    }

    res
}

def hello(): Unit = {
    val input = "354"
    println(s"Part One: ${evaluatorOne(input)}")
    println(s"Part Two: ${evaluatorTwo(input)}")
}