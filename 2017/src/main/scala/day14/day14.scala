package day14

import scala.collection.mutable.Queue

def knotHash(input: String): List[Int] = {
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
  
    return output.grouped(16).map(_.reduce(_ ^ _)).toList
}

def extract(input: String): Seq[List[Char]] = {
    return (0 until 128).map { y =>
        knotHash(s"$input-$y").flatMap { n =>
            (7 to 0 by -1).map { bit => if ((n & (1 << bit)) != 0) '#' else '.' }
        }
    }
}

def fill(mtx: Array[Array[Char]], startCell: (Int, Int)): Unit = {
    val q = Queue(startCell)
    val (rows, cols) = (mtx.length, mtx(0).length)

    while (q.nonEmpty) do {
        val (i, j) = q.dequeue()
        mtx(i)(j) = ' '

        val neighbors = for {
            (di, dj) <- Seq((-1, 0), (1, 0), (0, -1), (0, 1))
            ni = i + di
            nj = j + dj
            if (ni >= 0 && ni < rows && nj >= 0 && nj < cols && mtx(ni)(nj) == '#')
        } yield (ni, nj)

        q.enqueueAll(neighbors)
    }
}

def evaluatorOne(input: String): Int = extract(input).flatten.count(_ == '#')

def evaluatorTwo(input: String): Int = {
    val mtx = extract(input).map(_.toArray).toArray
    var regions = 0

    for {
        i <- mtx.indices
        j <- mtx(0).indices
        if mtx(i)(j) == '#'
    } do {
        regions += 1
        fill(mtx, (i, j))
    }

    return regions
}

def hello(): Unit = {
    val inputLine = "jxqlasbh"
    println(s"Part One: ${evaluatorOne(inputLine)}")
    println(s"Part Two: ${evaluatorTwo(inputLine)}")
}