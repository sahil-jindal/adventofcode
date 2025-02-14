package day17

import scala.collection.mutable.ListBuffer

def evaluatorOne(step: Int): Int = {
    val nums = ListBuffer(0)
    var pos = 0

    for (i <- 1 until 2018) {
        pos = (pos + step) % nums.length + 1
        nums.insert(pos, i)
    }

    return nums((pos + 1) % nums.length)
}

def evaluatorTwo(step: Int): Int = {
    var pos = 0
    var numsCount = 1
    var res = 0

    for (i <- 1 until 50000001) {
        pos = (pos + step) % numsCount + 1
        if (pos == 1) res = i
        numsCount += 1
    }

    return res
}

def hello(): Unit = {
    val input = 354
    println(s"Part One: ${evaluatorOne(input)}")
    println(s"Part Two: ${evaluatorTwo(input)}")
}