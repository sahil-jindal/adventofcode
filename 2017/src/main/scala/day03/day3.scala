package day03

import scala.collection.mutable.Map

def getCoordinates(n: Int): (Int, Int) = {
    if (n == 1) return (0, 0)
    
    val ringNo = math.ceil((math.sqrt(n) - 1) / 2).toInt
    val start = (2 * ringNo - 1) * (2 * ringNo - 1)
    val offset = n - start - 1

    if (offset < 2 * ringNo) {
        return (ringNo, -ringNo + 1 + offset)
    }

    if (offset < 4 * ringNo) {
        val offsetSide = offset - 2 * ringNo
        return (ringNo - (offsetSide + 1), ringNo)
    }
    
    if (offset < 6 * ringNo) {
        val offsetSide = offset - 4 * ringNo
        return (-ringNo, ringNo - (offsetSide + 1))
    }
    
    val offsetSide = offset - 6 * ringNo
    return (-ringNo + (offsetSide + 1), -ringNo)
}

def firstSumAbove(input: Int): Int = {
    if (input == 1) return 1
    
    val grid = Map[(Int, Int), Int]()
    grid((0, 0)) = 1

    var n = 2
    
    while (true) {
        val (x, y) = getCoordinates(n)
        var sum = 0

        for (dx <- -1 to 1; dy <- -1 to 1 if !(dx == 0 && dy == 0)) {
            sum += grid.getOrElse((x + dx, y + dy), 0)
        }

        if (sum > input) return sum
        
        grid((x, y)) = sum
        n += 1
    }

    return -1 // Unreachable
}

def evaluatorOne(input: Int): Int = {
    val (x, y) = getCoordinates(input)
    return x.abs + y.abs
}

def evaluatorTwo(input: Int): Int = firstSumAbove(input)
 
def hello(): Unit = {
    val input = 265149
    println(s"Part One: ${evaluatorOne(input)}")
    println(s"Part Two: ${evaluatorTwo(input)}")
}