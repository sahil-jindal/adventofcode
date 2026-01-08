package day23

def solve(digits: IndexedSeq[Int], maxLabel: Int, rotate: Int): LazyList[Long] = {
    val next = (1 to maxLabel + 1).toArray
    next(0) = -1

    val rightShifted = digits.tail :+ digits.head
    (digits zip rightShifted).foreach { case (d1, d2) => next(d1) = d2 }
    
    if (maxLabel > digits.size) {
        next(maxLabel) = next(digits.last)
        next(digits.last) = digits.size + 1
    }

    var current = digits.head

    for (i <- 0 until rotate) {
        val removed1 = next(current)
        val removed2 = next(removed1)
        val removed3 = next(removed2)
        next(current) = next(removed3)

        var destination = current - 1

        while (destination < 1 || Set(removed1, removed2, removed3).contains(destination)) {
            destination = if (destination <= 1) maxLabel else destination - 1
        }

        next(removed3) = next(destination)
        next(destination) = removed1
        current = next(current)
    }

    return LazyList.iterate(next(1))(next).map(_.toLong)
}

def evaluatorOne(digits: IndexedSeq[Int]): String = solve(digits, 9, 100).take(8).mkString
def evaluatorTwo(digits: IndexedSeq[Int]): Long = solve(digits, 1000000, 10000000).take(2).product

def hello(): Unit = {
    val inputLine = "418976235"
    val digits = inputLine.map(_.asDigit)
    println(s"Part One: ${evaluatorOne(digits)}")
    println(s"Part Two: ${evaluatorTwo(digits)}")
}