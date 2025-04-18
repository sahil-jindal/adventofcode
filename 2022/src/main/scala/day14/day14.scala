package day14

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable
import scala.util.control.Breaks._

case class Complex(re: Int, im: Int) {
    def +(other: Complex): Complex = Complex(re + other.re, im + other.im)
}

class Cave(input: List[String], hasFloor: Boolean) {
    private val map = mutable.Map.empty[Complex, Char]

    private val maxImaginary: Int = {
        for(line <- input) {
            val steps = line.split(" -> ").map(step => {
                val Array(x, y) = step.split(",").map(_.toInt)
                Complex(x, y)
            })

            for (i <- 1 until steps.length) {
                fillWithRocks(steps(i - 1), steps(i))
            }
        }
        
        map.keys.map(_.im).max
    }

    def fillWithRocks(from: Complex, to: Complex): Int = {
        val dir = Complex((to.re - from.re).sign, (to.im - from.im).sign)
        var pos = from
        var steps = 0

        while (pos != to + dir) {
            map(pos) = '#'
            pos += dir
            steps += 1
        }
        
        steps
    }

    def fillWithSand(sandSource: Complex): Int = {
        breakable {
            while (true) {
                val location = simulateFallingSand(sandSource)

                // already has sand there
                if (map.contains(location)) {
                    break()
                }

                // flows into the void
                if (!hasFloor && location.im == maxImaginary + 1) {
                    break()
                }

                map(location) = 'o'
            }
        }
        
        return map.values.count(_ == 'o')
    }

    def simulateFallingSand(sand: Complex): Complex = {
        val down = Complex(0, 1)
        val left = Complex(-1, 1)
        val right = Complex(1, 1)

        var current = sand

        breakable {
            while (current.im < maxImaginary + 1) {
                if (!map.contains(current + down)) {
                    current += down
                } else if (!map.contains(current + left)) {
                    current += left
                } else if (!map.contains(current + right)) {
                    current += right
                } else {
                    break()
                }
            }
        }

        current
    }
}

def evaluatorOne(input: List[String]): Int = new Cave(input, false).fillWithSand(Complex(500, 0))
def evaluatorTwo(input: List[String]): Int = new Cave(input, true).fillWithSand(Complex(500, 0))

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day14.txt") match {
        case Success(lines) => {
            println(s"Part One: ${evaluatorOne(lines)}")
            println(s"Part Two: ${evaluatorTwo(lines)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}