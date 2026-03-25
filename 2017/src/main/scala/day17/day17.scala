package day17

import scala.util.control.Breaks._

extension (a: Int) {
    def divCeil(b: Int): Int = {
        val (d, r) = (a / b, a % b)
        if (r == 0) return d
        if ((a ^ b) >= 0) return d + 1
        return d
    }
}

def solver(input: Int): (Int, Int) = {
    val step = input + 1
    
    def helper(index: Int, len: Int) = (index + step) % len

    val indexes = (1 to 2017).scanLeft(0)(helper).tail
    var index = indexes.last
    
    var next = (index + 1) % 2017
    var partOne = 0
    
    breakable {
        for ((o, i) <- indexes.zipWithIndex.reverse) {
            if (o == next) {
                partOne = i + 1
                break()
            }
            if (o < next) { 
                next -= 1 
            }
        }
    }

    var n = 2017
    var partTwo = 0

    while (n <= 50_000_000) {
        if (index == 0) { partTwo = n }
        val skip = (n - index).divCeil(step - 1)
        n += skip
        index = index + skip * step - n
    }

    return (partOne, partTwo)
}

def hello(): Unit = {
    val inputLine = 354
    val (partOne, partTwo) = solver(inputLine)
    println(s"Part One: $partOne")
    println(s"Part Two: $partTwo")
}