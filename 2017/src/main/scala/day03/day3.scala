package day03

import scala.collection.mutable.Map

def getCoordinates(n: Int): (Int, Int) = {
    if (n == 1) return (0, 0)

    val ringNo = math.ceil((math.sqrt(n) - 1) / 2).toInt
    val start = (2 * ringNo - 1) * (2 * ringNo - 1)
    val offset = n - start - 1
    val side = offset / (2 * ringNo)
    val pos = offset % (2 * ringNo)

    return side match {
        case 0 => (ringNo, -ringNo + 1 + pos)
        case 1 => (ringNo - (pos + 1), ringNo)
        case 2 => (-ringNo, ringNo - (pos + 1))
        case _ => (-ringNo + (pos + 1), -ringNo)
    }
}

def firstSumAbove(input: Int): Int = {
    if (input == 1) return 1
    
    val grid = Map(((0, 0), 1))

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
    val inputLine = 265149
    println(s"Part One: ${evaluatorOne(inputLine)}")
    println(s"Part Two: ${evaluatorTwo(inputLine)}")
}