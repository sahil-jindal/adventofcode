package day23

def solve(input: String, maxLabel: Int, rotate: Int): LazyList[Long] = {
    val digits = input.map(_.asDigit)

    val next = (1 to maxLabel + 1).toArray
    next(0) = -1

    for (i <- digits.indices) {
        next(digits(i)) = digits((i + 1) % digits.size)
    }

    if (maxLabel > input.size) {
        next(maxLabel) = next(digits.last)
        next(digits.last) = input.size + 1
    }

    var current = digits.head

    for (i <- 0 until rotate) {
        val removed1 = next(current)
        val removed2 = next(removed1)
        val removed3 = next(removed2)
        next(current) = next(removed3)

        var destination = current - 1

        while (destination < 1 || destination == removed1 || destination == removed2 || destination == removed3) {
            destination = if (destination <= 1) maxLabel else destination - 1
        }

        next(removed3) = next(destination)
        next(destination) = removed1
        current = next(current)
    }

    return LazyList.iterate(next(1))(next).dropRight(1).map(_.toLong)
}

def evaluatorOne(input: String): String = solve(input, 9, 100).take(8).mkString("")

def evaluatorTwo(input: String): Long = {
    val labels = solve(input, 1000000, 10000000).take(2)
    return labels(0) * labels(1)
}

def hello(): Unit = {
    val inputLine = "418976235"
    println(s"Part One: ${evaluatorOne(inputLine)}")
    println(s"Part Two: ${evaluatorTwo(inputLine)}")
}