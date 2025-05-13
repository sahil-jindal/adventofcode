package day14

import scala.collection.mutable.Queue

case class Point(y: Int, x: Int)

def knotHash(input: String): Seq[Int] = {
    val suffix = Seq(17, 31, 73, 47, 23)
    val chars = input.map(_.toInt) ++ suffix
    val output = (0 until 256).toArray
    var current = 0
    var skip = 0

    for (_ <- 0 until 64) do {
        for (len <- chars) do {
            for (i <- 0 until len / 2) do {
                val from = (current + i) % output.length
                val to = (current + len - 1 - i) % output.length
                val temp = output(from)
                output(from) = output(to)
                output(to) = temp
            }
            
            current += len + skip
            skip += 1
        }
    }
  
    return output.grouped(16).map(_.reduce(_ ^ _)).toSeq
}

def extract(input: String): Seq[Seq[Char]] = {
    return (0 until 128).map { y =>
        knotHash(s"$input-$y").flatMap { n =>
            (7 to 0 by -1).map { bit => if ((n & (1 << bit)) != 0) '#' else '.' }
        }
    }
}

def getNeighbours(pos: Point) = Seq(
    pos.copy(x = pos.x - 1),
    pos.copy(x = pos.x + 1),
    pos.copy(y = pos.y - 1),
    pos.copy(y = pos.y + 1)
)

def fill(mtx: Array[Array[Char]], startCell: Point): Unit = {
    val q = Queue(startCell)
    val (rows, cols) = (mtx.length, mtx(0).length)

    while (q.nonEmpty) {
        val pos = q.dequeue()
        mtx(pos.y)(pos.x) = ' '

        val neighbors = getNeighbours(pos).filter { n =>
            n.y >= 0 && n.y < rows && n.x >= 0 && n.x < cols && mtx(n.y)(n.x) == '#'
        }

        q.enqueueAll(neighbors)
    }
}

def evaluatorOne(input: Seq[Seq[Char]]): Int = input.flatten.count(_ == '#')

def evaluatorTwo(input: Seq[Seq[Char]]): Int = {
    val mtx = input.map(_.toArray).toArray
    var regions = 0

    for {
        y <- mtx.indices
        x <- mtx(0).indices
        if mtx(y)(x) == '#'
    } do {
        regions += 1
        fill(mtx, Point(y, x))
    }

    return regions
}

def hello(): Unit = {
    val inputLine = "jxqlasbh"
    val hashed = extract(inputLine)
    println(s"Part One: ${evaluatorOne(hashed)}")
    println(s"Part Two: ${evaluatorTwo(hashed)}")
}