package day13

import scala.collection.mutable.Queue

case class Triplet(x: Int, y: Int, stepCount: Int)

def solver(favorite: Int): (Int, Int) = {
    val maze = Array.ofDim[Boolean](52, 52)

    for (x <- 0 to 51; y <- 0 to 51) {
        val n = x*x + 3*x + 2*x*y + y + y*y + favorite
        maze(x)(y) = Integer.bitCount(n) % 2 == 0
    }

    var (partOne, partTwo) = (0, 0)

    val todo = Queue(Triplet(1, 1, 0))
    maze(1)(1) = false

    while (todo.nonEmpty) {
        val Triplet(x, y, cost) = todo.dequeue()

        if (x == 31 && y == 39) {
            partOne = cost
        }

        if (cost <= 50) {
            partTwo += 1
        }

        if (x > 0 && maze(x - 1)(y)) {
            todo.enqueue(Triplet(x - 1, y, cost + 1))
            maze(x - 1)(y) = false
        }

        if (y > 0 && maze(x)(y - 1)) {
            todo.enqueue(Triplet(x, y - 1, cost + 1))
            maze(x)(y - 1) = false
        }

        if (x < 51 && maze(x + 1)(y)) {
            todo.enqueue(Triplet(x + 1, y, cost + 1))
            maze(x + 1)(y) = false
        }

        if (y < 51 && maze(x)(y + 1)) {
            todo.enqueue(Triplet(x, y + 1, cost + 1))
            maze(x)(y + 1) = false
        }
    }

    return (partOne, partTwo)
}

def hello(): Unit = {
    val input = 1350
    val (partOne, partTwo) = solver(input)
    println(s"Part One: ${partOne}")
    println(s"Part Two: ${partTwo}")
}