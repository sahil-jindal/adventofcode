package day14

def knotHash(input: String): List[Int] = {
    val suffix = List(17, 31, 73, 47, 23)
    val chars = input.map(_.toInt).toList ++ suffix
    var knot = (0 until 256).toVector
    var (position, skip) = (0, 0)

    for (_ <- 0 until 64; len <- chars) {
        val next = len + skip

        // Reverse the vector from 0 until length
        val (a, b) = knot.splitAt(len)
        knot = a.reverse ++ b

        // Rotating the vector to the left by (next % 256)
        val (c, d) = knot.splitAt(next % 256)
        knot = d ++ c

        position += next
        skip += 1
    }
  
    val temp = position % 256
    val hash = knot.takeRight(temp) ++ knot.dropRight(temp)

    return hash.grouped(16).map(_.reduce(_ ^ _)).toList
}

def extract(input: String): IndexedSeq[List[Boolean]] = {
    return (0 until 128).map { y =>
        knotHash(s"$input-$y").flatMap { n =>
            (7 to 0 by -1).map { bit => (n & (1 << bit)) != 0 }
        }
    }
}

def fill(grid: Array[Array[Boolean]], y: Int, x: Int): Unit = {
    grid(y)(x) = false

    if (x > 0 && grid(y)(x - 1)) {
        fill(grid, y, x - 1)
    }

    if (x < 127 && grid(y)(x + 1)) {
        fill(grid, y, x + 1)
    }

    if (y > 0 && grid(y - 1)(x)) {
        fill(grid, y - 1, x)
    }

    if (y < 127 && grid(y + 1)(x)) {
        fill(grid, y + 1, x)
    }
}

def evaluatorOne(input: IndexedSeq[List[Boolean]]): Int = input.flatten.count(identity)

def evaluatorTwo(input: IndexedSeq[List[Boolean]]): Int = {
    val mtx = input.map(_.toArray).toArray
    var regions = 0

    for {
        y <- 0 to 127
        x <- 0 to 127
        if mtx(y)(x)
    } do {
        regions += 1
        fill(mtx, y, x)
    }

    return regions
}

def hello(): Unit = {
    val inputLine = "jxqlasbh"
    val hashed = extract(inputLine)
    println(s"Part One: ${evaluatorOne(hashed)}")
    println(s"Part Two: ${evaluatorTwo(hashed)}")
}