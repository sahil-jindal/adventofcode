package day10

import scala.util.{Try, Success, Failure, Using}
import scala.io.Source
import scala.collection.mutable.Map

case class SinglePress(buttonCount: Int, joltageChange: List[Int])

case class Problem(target: List[Int], buttons: List[Int], joltage: List[Int]) {
    def singlePresses: IndexedSeq[SinglePress] = {
        return (0 until 1 << buttons.size).map(buttonMask => {
            val joltageChange = Array.ofDim[Int](joltage.length)
            var buttonCount = 0

            for ((button, ibutton) <- buttons.zipWithIndex) {
                if ((buttonMask >> ibutton) % 2 == 1) {
                    buttonCount += 1
                    for (ijoltage <- joltage.indices) {
                        if ((button >> ijoltage) % 2 == 1) {
                            joltageChange(ijoltage) += 1
                        }
                    }
                }
            }

            SinglePress(buttonCount, joltageChange.toList)
        })
    }
}

def parseInput(input: List[String]) = input.map(line => {
    val parts = line.split(" ").toList

    val target = parts.head.collect {
        case '#' => 1 
        case '.' => 0
    }

    val buttons = parts.slice(1, parts.size - 1).map(part => {
        raw"\d+".r.findAllIn(part).map(it => 1 << it.toInt).sum
    })

    val joltage = raw"\d+".r.findAllIn(parts.last).map(_.toInt).toList

    Problem(target.toList, buttons, joltage)
})

def solve(joltages: List[Int], singlePresses: IndexedSeq[SinglePress], cache: Map[String, Int]): Int = {
    if (joltages.forall(_ == 0)) return 0

    val key = joltages.mkString("-")

    return cache.getOrElseUpdate(key, {
        var best = 10_000_000

        for (SinglePress(buttonCount, joltageChange) <- singlePresses) {

            val evens = (joltages zip joltageChange).forall {
                case (j, jC) => j >= jC && (j - jC) % 2 == 0
            }

            if (evens) {
                val subProblem = (joltages zip joltageChange).map {
                    case (j, jC) => (j - jC) / 2
                }

                best = Math.min(best, buttonCount + 2*solve(subProblem, singlePresses, cache))
            } 
        }

        best
    })
}

def evaluatorOne(input: List[Problem]) = input.map(p => {
    p.singlePresses
        .withFilter(press => p.target.sameElements(press.joltageChange.map(_ % 2)))
        .map(_.buttonCount).min
}).sum

def evaluatorTwo(input: List[Problem]) = input.map(p => {
    solve(p.joltage, p.singlePresses, Map.empty)
}).sum

def readLinesFromFile(filePath: String): Try[List[String]] =
    Using(Source.fromResource(filePath))(_.getLines().toList)

def hello(): Unit = {
    readLinesFromFile("day10.txt") match {
        case Success(lines) => {
            val input = parseInput(lines)
            println(s"Part One: ${evaluatorOne(input)}")
            println(s"Part Two: ${evaluatorTwo(input)}")
        }
        case Failure(exception) => {
            println(s"Error reading file: ${exception.getMessage}")
        }
    }
}